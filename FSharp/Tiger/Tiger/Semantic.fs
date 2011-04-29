module Semantic
open Ast

// types used by the semantic analyzer 
type TypeConstants = 
| Int
| String
| Object
| Array of TypeConstants
| Record of (string * TypeConstants) list
| Function of (TypeConstants list * TypeConstants)
| Unit // a.k.a void
| Undefined_Type

let rec getDefaultValue t = 
    match t with
    | Int -> 0 :> System.Object
    | _ -> null

let rec getTypeName t = 
    match t with 
    | Int -> "int"
    | String -> "string"
    | Object -> "obj"
    | Array(t') -> sprintf "%s[]"  (getTypeName t')
    | Record(fields) -> let recordFields = fields 
                                           |> List.map (fun (f,t') -> sprintf "%s:%s" f (getTypeName t')) 
                                           |> String.concat "; "
                        sprintf "{%s}" recordFields
    | Function (parameters, returnType) ->
                        let parametersStr = parameters
                                            |> List.map (fun (p) -> sprintf "%s" (getTypeName p))
                                            |> String.concat "->"
                        let returnTypeStr = getTypeName returnType
                                            
                        sprintf "%s->%s" parametersStr returnTypeStr 
    | Unit -> "unit"
    | Undefined_Type -> "undefined"

let rec getTypeFromName n =
    match n with
    | "unit"      -> Unit
    | "undefined" -> Undefined_Type
    | "int" -> Int
    | "string" -> String
    | "obj" -> Object
    | a when a.EndsWith("[]") ->
                let baseType = a.Substring(0, a.Length - 2)
                Array (getTypeFromName baseType)

    | r when r.EndsWith("}") -> 
                let fieldsDef = r.Substring (1, r.Length - 2)
                let fields = fieldsDef.Split([|';'|]) 
                                        |> Array.map (fun f -> f.Trim()) 
                                        |> Array.map (fun f -> let parts = f.Split([|':'|])
                                                               (parts.[0].Trim(), parts.[1].Trim() |> getTypeFromName))
                                        |> Array.toList
                Record(fields)
    
    | f when f.Contains("->") ->
                let rt :: p = f.Split ([|"->"|], System.StringSplitOptions.RemoveEmptyEntries) 
                                    |> Array.rev 
                                    |> Array.toList 
                let pTypes = p |> List.map getTypeFromName
                let rtType = rt |> getTypeFromName
                Function (pTypes, rtType)
                          
    | _ -> failwith (sprintf "Invalid type name %s" n)

type TypeDefinition = 
| Alias of string (* another type name*)
| Record of ((string * string) list) (* list of field names and field types *)
| Array of string (* base array type *)

type TigerDeclaration = 
| TypeDeclaration of string (* name *) 
                   * (TypeConstants option) (* alias / fields / array*)

| VariableDeclaration of string (* name *) 
                       * (TypeConstants) (* type name *)
                       * (System.Object -> System.Object) (* context-bound value *)

| FunctionDeclaration of string (* name *)
                        * ((string * TypeConstants) list) (* parameters *) 
                        * (TypeConstants) (* return type *) 
                        * (System.Object -> System.Object) (* delegate returning value *)
with member this.IsNamed (name) =
            match this with
            | TypeDeclaration (n,_) -> n = name
            | VariableDeclaration (n, _, _) -> n = name
            | FunctionDeclaration (n, _, _, _) -> n = name 

type Scope(parent: Scope option, isLoopScope: bool) = 
    let parent = parent 
    let isLoopScope = isLoopScope
    let mutable declarations : TigerDeclaration list = []
    let mutable typeTable = [
                             // predefined types
                             ("int", Int);
                             ("string", String)
                             // predefined functions
                             ("print", Function ([String], Unit))
                             ("printi", Function ([Int], Unit))
                             ("flush", Function ([], Unit))
                             ("getchar", Function ([], String))
                             ("ord", Function ([String], Int))
                             ("chr", Function ([Int], String))
                             ("size", Function ([String], Int))
                             ("substring", Function ([String;Int;Int], String))
                             ("concat", Function ([String;String], String))
                             ("not", Function ([Int], Int))
                             ("exit", Function ([Int], Unit))
                            ] |> Map.ofList

    new() = Scope(None, false)
    member this.ChildScope(isLoopScope) = Scope(Some(this), isLoopScope)
    member this.IsLoopScope = isLoopScope
    member this.IsTopMost = parent.IsNone

    member this.Add (d: TigerDeclaration): unit =
        declarations <- declarations @ [d] // Scope.Add
        match d with
        
        | VariableDeclaration (name, typeDef, _ ) -> typeTable <- typeTable.Add (name, typeDef)
        
        | TypeDeclaration (name, typeDef) -> let finalType = match typeDef with None -> Undefined_Type | Some(t') -> t'
                                             typeTable <- typeTable.Add (name, finalType)
        
        | FunctionDeclaration(funcName, parameters, typeDef, funcEvaluator) -> typeTable <- typeTable.Add (funcName, typeDef)

    member this.AddRange (r: TigerDeclaration list): unit = 
            r |> List.iter this.Add
            ()
    
    member this.Contains (name: string) =
        let inThisScope =  declarations |> List.exists (fun d -> d.IsNamed name)
        if not inThisScope 
        then match parent with
                | None -> false
                | Some (scope) -> scope.Contains(name)
        else true // Scope.Contains

    member this.InferTypeForName (name:string) = 
        let typeHere = typeTable.TryFind name     
        match typeHere with
        | None -> match parent with None -> None | Some(scope) -> scope.InferTypeForName name
        | _ -> typeHere

type ParserErrors =
| T001_TypeMismatch
| T002_IdentifierNotFound
| T003_ExpectedRecordType
| T004_InvalidFieldForRecordType
| T005_ExpectedArrayType
| T006_ExpectedValuedExpression
| T007_ExpectedArray
| T008_ExpectedFunction
| T009_IfThenElseBranchesTypeMismatch
| T010_InvalidFunctionCallParameter
| T011_BreakOutsideLoop
| T012_DuplicateDefinitionOfValue
| T014_InvalidFunctionParameterDefinition
| T015_InvalidTypeForRecordField
| UnknownError

let reportParserError error errorDetails : unit = 
    let errorStr =  match error with
                    | T001_TypeMismatch -> 
                        System.String.Format ("T001: Types mismatch. Expected '{0}', found '{1}'", errorDetails |> List.toArray)
                    | T002_IdentifierNotFound -> 
                       System.String.Format ("T002: Identifier '{0}' was not found", errorDetails |> List.toArray)
                    | T003_ExpectedRecordType ->
                        System.String.Format ("T003: Expression '{0}' should be of type Record to use the '.' operator", errorDetails |> List.toArray)
                    | T004_InvalidFieldForRecordType ->
                        System.String.Format ("T004: Invalid field '{0}' for record type '{1}'", errorDetails |> List.toArray)
                    | T005_ExpectedArrayType ->
                        System.String.Format ("T005: Expression '{0}' should be of type array to use the '[]' operator", errorDetails |> List.toArray)
                    | T006_ExpectedValuedExpression ->
                        System.String.Format ("T006: Expression '{0}' should return a value to be able to be used in an assignment or array definition or record definition", errorDetails |> List.toArray)
                    | T007_ExpectedArray ->
                        System.String.Format ("T007: Expression '{0}' should be of type array", errorDetails |> List.toArray)
                    | T008_ExpectedFunction ->
                        System.String.Format ("T008: The name '{0}' does not denote a function", errorDetails |> List.toArray)
                    | T009_IfThenElseBranchesTypeMismatch ->
                        System.String.Format ("T009: The 'then' and 'else' branches types mismatch. Found '{0}' and '{1}'", errorDetails |> List.toArray)
                    | T010_InvalidFunctionCallParameter ->
                        System.String.Format ("T010: Invalid argument # '{0}' to function '{1}'. Expected '{2}', found '{3}'", errorDetails |> List.toArray)
                    | T011_BreakOutsideLoop ->
                        System.String.Format ("T011: Break expression outside loop", [| |])
                    | T012_DuplicateDefinitionOfValue ->
                        System.String.Format ("T012: Duplicate definition of value '{0}'", errorDetails |> List.toArray)                    
                    | T014_InvalidFunctionParameterDefinition ->
                        System.String.Format ("T014: Function '{0}' has some invalid parameters: {1}", errorDetails |> List.toArray)
                    | T015_InvalidTypeForRecordField ->
                        System.String.Format ("T015: Type '{0}' in record definition is not defined", errorDetails |> List.toArray)                     
                    | _ -> "Unknown error"
    failwith errorStr


let evaluateExpr (e, context) = 
    match e with
    | _ -> null : System.Object

let rec inferTypeFromIdentifier i (scope:Scope) =
    let (Identifier(name)) = i
    let typeForName = scope.InferTypeForName name
    match typeForName with
    | None -> reportParserError ParserErrors.T002_IdentifierNotFound [name]
              None
    | Some (t) -> Some(t) 

and inferTypeFromLValue lvalue scope = 
    match lvalue with
    | LValueId (id) -> inferTypeFromIdentifier id scope
    | MemberAccess (l, id) -> 
        let name = getIdentifierName id
        let lvalueType = inferTypeFromLValue l scope
        match lvalueType with
        | None -> None
        | Some (t) -> match t with
                      | TypeConstants.Record (fields) -> 
                            let fieldDef = fields |> List.tryFind (fun (f,t) -> f = name)  
                            match fieldDef with
                            | None -> reportParserError ParserErrors.T004_InvalidFieldForRecordType [name; getTypeName t]
                                      None
                            | Some ((f,tField)) -> Some (tField)

                      | _ -> reportParserError ParserErrors.T003_ExpectedRecordType [l]
                             None

    | ArrayAccess (l, expr) ->
        let lvalueType = inferTypeFromLValue l scope
        match lvalueType with
        | None -> None
        | Some (t) -> match t with
                      | TypeConstants.Array (baseType) -> Some (baseType)
                      | _ -> reportParserError ParserErrors.T005_ExpectedArrayType [l]
                             None

and inferTypeFromValue v scope = 
    match v with
    | Integer (_) -> Some (Int)
    | Str (_) -> Some (String)
    | Nil -> Some (Object)
    | Id (i) -> inferTypeFromIdentifier i scope
    | Negative (expr) -> inferTypeFromExpr expr scope
    | ParenExp (expr) -> inferTypeFromExpr expr scope
    | LValue (l) -> inferTypeFromLValue l scope

and inferTypeFromFactor f scope =
    match f with
    | Value (v) -> inferTypeFromValue v scope
    | NegativeValue (v) -> inferTypeFromValue v scope

and checkFactorAndTermMatch factor term scope = 
    let factorType = inferTypeFromFactor factor scope    
    let termType   = inferTypeFromTerm term scope
    if factorType <> termType 
    then 
        reportParserError ParserErrors.T001_TypeMismatch [termType; factorType]
        factorType // a local correction to try to capture other possible errors
    else 
        factorType

and inferTypeFromTerm t scope = 
    match t with
    | Factor (f) -> inferTypeFromFactor f scope
    | Multiply (f, t) -> checkFactorAndTermMatch f t scope
    | Divide (f,t) -> checkFactorAndTermMatch f t scope

and checkTermAndExpMatch term exp scope = 
    let termType = inferTypeFromTerm term scope
    let expType = inferTypeFromExpr exp scope
    if termType <> expType 
    then 
        reportParserError ParserErrors.T001_TypeMismatch [termType; expType]
        termType // a local correction to try to capture other possible errors
    else match termType with
         | Some (Int) -> Some (Int)
         | _ -> reportParserError ParserErrors.T001_TypeMismatch ["int"; termType]
                None

and inferTypeFromExpr e scope = 
    match e with
    | Plus (t,exp) -> checkTermAndExpMatch t exp scope 
    | Minus (t, exp) -> checkTermAndExpMatch t exp scope
    | And (t, exp) -> checkTermAndExpMatch t exp scope
    | Or (t, exp) -> checkTermAndExpMatch t exp scope
    | Equals (t, exp) -> checkTermAndExpMatch t exp scope
    | NotEquals (t, exp) -> checkTermAndExpMatch t exp scope
    | GreaterThan (t, exp) -> checkTermAndExpMatch t exp scope
    | GreaterThanEqual (t, exp) -> checkTermAndExpMatch t exp scope
    | LessThan (t, exp) -> checkTermAndExpMatch t exp scope
    | LessThanEqual (t, exp) -> checkTermAndExpMatch t exp scope
    | Term (t) -> inferTypeFromTerm t scope
    | Expr.Array (id, indexer , value) ->
            // the underying type of the id should be array 
            let idType = inferTypeFromIdentifier id scope
            let underlyingType = match idType with
                                    | Some (TypeConstants.Array (baseType)) -> Some (baseType)
                                    | _ -> reportParserError ParserErrors.T007_ExpectedArray [getIdentifierName id]
                                           None

            // the indexer expression has to be an int
            let indexerType = inferTypeFromExpr indexer scope
            match indexerType with
            | Some (Int) -> ()
            | _ -> reportParserError ParserErrors.T006_ExpectedValuedExpression [indexer]

            // the value type of the expression should be the same as the underlying type
            let valueType = inferTypeFromExpr value scope
            match valueType with
            | Some (t) -> if underlyingType.IsSome && t <> underlyingType.Value
                          then 
                                reportParserError ParserErrors.T001_TypeMismatch [underlyingType.Value;t]
                                None
                          else
                                Some ( TypeConstants.Array(t) )
            | None     -> reportParserError ParserErrors.T006_ExpectedValuedExpression [value]
                          None
            

    | Expr.Record(id, fieldList) -> 
            let pairs = fieldList |> List.mapi 
                                     (
                                        fun i (FieldAssignment (id, fieldExpr)) -> 
                                           (i, getIdentifierName (id), inferTypeFromExpr fieldExpr scope)
                                     )
            let invalidRecordFields = pairs |> List.filter (fun (index, s,t) -> t = None )
            if List.isEmpty invalidRecordFields
            then 
                let recordFields = pairs |> List.map (fun (_, s, Some(t')) -> (s, t'))
                Some (TypeConstants.Record(recordFields) )
            else
                None
    | Expr.Assign (lvalue, expr) -> 
        let exprType = inferTypeFromExpr expr scope
        match exprType with
        | None -> reportParserError ParserErrors.T006_ExpectedValuedExpression [expr]
                  None
        
        | _     -> None

    | Expr.FunctionCall (id, exprList) ->
        // the id has to be defined before and as a function
        let idType = inferTypeFromIdentifier id scope
        match idType with
        | Some ( TypeConstants.Function(parameters, returnType) ) ->
            // none of the expressions on the call site can be void
            let exprListTypes = exprList |> List.map (fun expr -> inferTypeFromExpr expr scope)
            let voidExprs = exprListTypes |> List.tryFind (fun eType -> eType.IsNone)
            match voidExprs with
            | None -> () // ok then
            | Some (v) -> reportParserError ParserErrors.T006_ExpectedValuedExpression [v]

            // each one of the expressions on the call site must match the parameter types
            let exprParamMatch = 
                List.zip parameters exprListTypes 
                |> List.mapi (fun i (t, exprT) -> (i, t,  exprT))
                |> List.filter (fun (i, t, exprT) ->  t <> exprT.Value)
            if not (List.isEmpty exprParamMatch)
            then 
                let (i, paramType, exprType) = exprParamMatch |> List.head
                let functionName = getIdentifierName id
                reportParserError ParserErrors.T001_TypeMismatch [i; functionName; paramType; exprType]
                None
            else
                Some (returnType)
        | _      -> reportParserError ParserErrors.T008_ExpectedFunction [getIdentifierName id]
                    None
    | Expr.ExprSeq (exprList) -> 
        
        let exprsTypes = exprList |> List.map (fun e -> inferTypeFromExpr e scope )
        exprsTypes |> List.rev |> List.head

    | Expr.Conditional (ifExpr, thenExpr, elseExpr) ->
        let ifExprType = inferTypeFromExpr ifExpr scope
        let thenExprType = inferTypeFromExpr thenExpr scope 
           
        // ifExpr should evaluate to int
        match ifExprType with
        | Some (Int) -> ()
        | _          -> reportParserError ParserErrors.T001_TypeMismatch ["int"; ifExprType ]

        match elseExpr with 
        | None -> Some (TypeConstants.Unit)
        | Some (e') -> // thenExpr and elseExpr must be of the same type
                    
                let elseExprType = inferTypeFromExpr e' scope
                if thenExprType <> elseExprType
                then 
                    reportParserError ParserErrors.T009_IfThenElseBranchesTypeMismatch [thenExprType;elseExprType] 
                    None
                else
                    thenExprType

    | Expr.While (expr, bodyExpr) ->
        let whileScope = scope.ChildScope(true)
        let condType = inferTypeFromExpr expr whileScope
        // conditional must evaluate to int
        match condType with
        | Some (Int) -> ()
        | _          -> reportParserError ParserErrors.T001_TypeMismatch ["int"; condType]
                        ()

        // body must be non-valued
        let bodyType = inferTypeFromExpr bodyExpr whileScope
        match bodyType with
        | None -> ()
        | _    -> reportParserError ParserErrors.T001_TypeMismatch ["unit"; bodyType]
                  ()
        Some (TypeConstants.Unit)
    
    |  Expr.For(id, loopStart, loopEnd, body) ->
            // loop start and end must be ints
            let lsType = inferTypeFromExpr loopStart scope
            let leType = inferTypeFromExpr loopEnd   scope

            match lsType with
            | Some (Int) -> ()
            | _          -> reportParserError ParserErrors.T001_TypeMismatch ["int"; lsType]
                            ()
            match leType with
            | Some (Int) -> ()
            | _          -> reportParserError ParserErrors.T001_TypeMismatch ["int"; leType]
                            () 

            // the for loop defines a new scope for its variable
            let varName = getIdentifierName id
            let varEvaluator x = x // dummy delegate
            let forScope = scope.ChildScope(true)
            forScope.Add (TigerDeclaration.VariableDeclaration (varName, Int, varEvaluator))

            // the loop body must be non-valued
            let bodyType = inferTypeFromExpr body forScope
            match bodyType with
            | Some (TypeConstants.Unit)  -> ()
            | _    -> reportParserError ParserErrors.T001_TypeMismatch ["unit"; bodyType]
                      ()
            Some (TypeConstants.Unit)    
              
    | Expr.Break ->          
            // break must be inside a loop scope
            if not scope.IsLoopScope
            then reportParserError ParserErrors.T011_BreakOutsideLoop []
            Some (TypeConstants.Unit)

    | Expr.Let (decLst, exprLst) ->
           // a let defines a new scope if the given scope is not the top most
           let letScope = if scope.IsTopMost then scope else scope.ChildScope(false)

           // convert all AST types to Semantic Analyzer types
           decLst |> List.map (fun d -> convertToTigerDeclaration letScope d) |> ignore
           
           let exprTypes = exprLst |> List.map (fun e -> inferTypeFromExpr e letScope)

           exprTypes |> List.rev |> List.head 


and getIdentifierName id = 
    match id with 
    | Identifier (name) -> name

and convertToTigerTypeDef (typeDef: Ast.TypeDef) = 
    match typeDef with
    | TypeIdDef (id) -> Alias(getIdentifierName (id))
    | TypeFieldsDef (typeFieldsLst) -> match typeFieldsLst with
                                       | None -> Record([])
                                       | Some (lst) -> 
                                           let mapTypeFields = lst |> List.map (fun (TypeField(id1, id2)) -> getIdentifierName id1,getIdentifierName id2)
                                           Record (mapTypeFields)

    | ArrayTypeDef (id) -> Array (getIdentifierName(id))


and convertToTigerDeclaration  (scope: Scope) (declaration : Ast.Declaration) =
    match declaration with
    | Declaration.TypeDeclaration (id, typeDef) -> 
        
        // validate there's no name clash
        let typeName = getIdentifierName id
        if scope.Contains typeName
        then reportParserError ParserErrors.T012_DuplicateDefinitionOfValue [typeName]

        let tigerTypeDef = typeDef |> convertToTigerTypeDef

        let realType = match tigerTypeDef with
                           // alias definition
                           | Alias (s) -> let aliasedType = scope.InferTypeForName s
                                          match aliasedType with
                                          | None -> reportParserError ParserErrors.T002_IdentifierNotFound [s]
                                          | _ -> ()
                                          (s, aliasedType)
                           // array of s
                           | Array (s) -> let underlyingType = scope.InferTypeForName s
                                          match underlyingType with
                                          | None -> reportParserError ParserErrors.T002_IdentifierNotFound [s]
                                                    (s, None)
                                          | Some (t) -> (s,Some (TypeConstants.Array(t)))
                           // { f1:t1, f2:t2,...,fk:tk}
                           | Record(fields) ->  
                                        let fieldMappings = 
                                            fields  |> List.map (fun (f,t) -> (f, t, scope.InferTypeForName t))
                                                    |> List.map 
                                                       (
                                                        fun (f, tName, t) -> 
                                                        match t with
                                                        | Some(t') -> (f, t')
                                                        | None     -> reportParserError ParserErrors.T015_InvalidTypeForRecordField [tName]
                                                                      (f, Undefined_Type)
                                                       )
                                        // the name here is irrelevant, what matters are the fields
                                        (getIdentifierName id, Some(TypeConstants.Record(fieldMappings)))

        let result = TigerDeclaration.TypeDeclaration (getIdentifierName id, snd realType)
        scope.Add result
        result

    | Declaration.VarDeclaration (varId, typeId, expr) ->
        
        let varName = getIdentifierName varId
        // validate there's no name clash
        if scope.Contains varName
        then reportParserError ParserErrors.T012_DuplicateDefinitionOfValue [varName]

        let typeDef = match typeId with
                        | None ->  let exprType = inferTypeFromExpr expr scope
                                   match exprType with
                                   | Some(t) -> Some (t)
                                   | None    -> reportParserError ParserErrors.T006_ExpectedValuedExpression [expr]
                                                None

                        | Some (id) -> inferTypeFromIdentifier id scope

        let result = TigerDeclaration.VariableDeclaration (varName, typeDef.Value, (fun context -> evaluateExpr (expr, context)) )
        scope.Add result
        result
    
    | Declaration.FuncDeclaration (funcId, typeFieldLst, typeId, expr) -> 
        
        let funcName = getIdentifierName funcId
        
        // validate there's no name clash
        if scope.Contains funcName
        then reportParserError ParserErrors.T012_DuplicateDefinitionOfValue [funcName]

        let parametersDef = match typeFieldLst with
                             | None -> []
                             | Some (lst) -> lst |> List.map (fun (TypeField(parameter, parameterType)) -> getIdentifierName parameter, parameterType)
                                                 |> List.map (fun (name, pType) -> (name, inferTypeFromIdentifier pType scope))

        // validate parameter types exists and are not unit
        let invalidParameters = parametersDef |> List.filter (fun (p,t) -> t = Some (TypeConstants.Unit))
        if not (List.isEmpty invalidParameters)
        then 
            let parameterReporter paramList = 
                paramList |> List.map (fun (p,t) -> p) 
                          |> String.concat ", "
            reportParserError ParserErrors.T014_InvalidFunctionParameterDefinition [funcName; (invalidParameters |> parameterReporter)]
        
        // a function defines a scope
        let funcScope = scope.ChildScope(false)
        let parameters = parametersDef |> List.filter (fun (p,t) -> t.IsSome) |> List.map(fun (p,Some(t)) -> (p,t))
        parameters |> List.iter (fun (p, t) -> funcScope.Add (VariableDeclaration (p, t, fun x -> x)))
        let parameterTypes = parameters |> List.map(fun (p,t) -> t)
         
        let specifiedType = match typeId with 
                            | None -> Some (TypeConstants.Unit) 
                            | Some(id) -> inferTypeFromIdentifier id scope        

        let typeDef = match typeId with
                        | None ->  
                            // when no type is given to a function definition, it is a procedure                            
                            Some (TypeConstants.Function (parameterTypes, TypeConstants.Unit)) 
                        
                        | Some(id) -> 
                             // the expr type must coincide with the specified type
                             Some (TypeConstants.Function (parameterTypes, specifiedType.Value))

        let result = TigerDeclaration.FunctionDeclaration (funcName, parameters , typeDef.Value, (fun context -> evaluateExpr (expr, context))) 
        scope.Add result

        // tricky this one, we have to define the function BEFORE validating 
        // it's body to allow for recursive definitions. nice one ;-)
        let funcBodyType = inferTypeFromExpr expr funcScope

        match typeId with
        | None ->  
            // the expr can't have a return type
            if funcBodyType.IsSome && funcBodyType.Value <> TypeConstants.Unit
            then reportParserError ParserErrors.T001_TypeMismatch ["unit"; funcBodyType]
                        
        | Some(id) -> 
                // the expr type must coincide with the specified type
                if funcBodyType <> specifiedType
                then reportParserError ParserErrors.T001_TypeMismatch [specifiedType; funcBodyType]
                

        result

and analyzeExpr e (scope: Scope) = inferTypeFromExpr e scope |> ignore

and analyze (Program(exp)) : unit =
    let programScope = new Scope() 
    analyzeExpr exp programScope 