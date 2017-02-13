module AST.Encoding exposing (encode, encodeInfix)

import AST.Ranges as Ranges exposing (Range)
import AST.Types exposing (..)
import Json.Encode as JE exposing (Value, object, int, string, list, float)
import Util.Json exposing (encodeTyped)


asList : (a -> Value) -> List a -> Value
asList f =
    list << List.map f


nameField : String -> ( String, Value )
nameField x =
    ( "name", string x )


rangeField : Range -> ( String, Value )
rangeField r =
    ( "range", Ranges.encode r )


encode : File -> Value
encode { moduleDefinition, imports, declarations } =
    object
        [ ( "moduleDefinition", encodeModule moduleDefinition )
        , ( "imports", asList encodeImport imports )
        , ( "declarations", asList encodeDeclaration declarations )
        ]


encodeModule : Module -> Value
encodeModule m =
    case m of
        NormalModule d ->
            encodeTyped "normal" (encodeDefaultModuleData d)

        PortModule d ->
            encodeTyped "port" (encodeDefaultModuleData d)

        EffectModule d ->
            encodeTyped "effect" (encodeEffectModuleData d)

        NoModule ->
            encodeTyped "nomodule" (JE.null)


encodeEffectModuleData : EffectModuleData -> Value
encodeEffectModuleData { moduleName, exposingList, command, subscription } =
    object
        [ ( "moduleName", encodeModuleName moduleName )
        , ( "exposingList", encodeExposingList exposingList encodeExpose )
        , ( "command", command |> Maybe.map string |> Maybe.withDefault JE.null )
        , ( "subscription", subscription |> Maybe.map string |> Maybe.withDefault JE.null )
        ]


encodeDefaultModuleData : DefaultModuleData -> Value
encodeDefaultModuleData { moduleName, exposingList } =
    object
        [ ( "moduleName", encodeModuleName moduleName )
        , ( "exposingList", encodeExposingList exposingList encodeExpose )
        ]


encodeModuleName : ModuleName -> Value
encodeModuleName =
    List.map string >> list


encodeExpose : Expose -> Value
encodeExpose exp =
    case exp of
        InfixExpose x r ->
            encodeTyped "infix" <|
                object
                    [ nameField x
                    , (rangeField r)
                    ]

        FunctionExpose x r ->
            encodeTyped "function" <|
                object
                    [ nameField x
                    , (rangeField r)
                    ]

        TypeOrAliasExpose x r ->
            encodeTyped "typeOrAlias" <|
                object
                    [ nameField x
                    , (rangeField r)
                    ]

        TypeExpose exposedType ->
            encodeTyped "typeexpose" (encodeExposedType exposedType)


encodeExposedType : ExposedType -> Value
encodeExposedType { name, constructors, range } =
    object
        [ (nameField name)
        , ( "inner", encodeExposingList constructors encodeValueConstructorExpose )
        , (rangeField range)
        ]


encodeValueConstructorExpose : ValueConstructorExpose -> Value
encodeValueConstructorExpose ( name, range ) =
    object
        [ (nameField name)
        , (rangeField range)
        ]


encodeExposingList : Exposure a -> (a -> Value) -> Value
encodeExposingList exp f =
    case exp of
        None ->
            encodeTyped "none" <| JE.null

        All r ->
            encodeTyped "all" <| Ranges.encode r

        Explicit l ->
            encodeTyped "explicit" (asList f l)


encodeImport : Import -> Value
encodeImport { moduleName, moduleAlias, exposingList, range } =
    object
        [ ( "moduleName", encodeModuleName moduleName )
        , ( "moduleAlias"
          , moduleAlias
                |> Maybe.map encodeModuleName
                |> Maybe.withDefault JE.null
          )
        , ( "exposingList", encodeExposingList exposingList encodeExpose )
        , (rangeField range)
        ]


encodeDeclaration : Declaration -> Value
encodeDeclaration decl =
    case decl of
        FuncDecl function ->
            encodeTyped "function" (encodeFunction function)

        AliasDecl typeAlias ->
            encodeTyped "typeAlias" (encodeTypeAlias typeAlias)

        TypeDecl typeDeclaration ->
            encodeTyped "typedecl" (encodeType typeDeclaration)

        PortDeclaration sig ->
            encodeTyped "port" (encodeSignature sig)

        InfixDeclaration inf ->
            encodeTyped "infix"
                (encodeInfix inf)

        DestructuringDeclaration x ->
            encodeTyped "destrucutring" (encodeDestructuring x)


encodeInfix : Infix -> Value
encodeInfix inf =
    object
        [ ( "direction", encodeInfixDirection inf.direction )
        , ( "precedence", int inf.precedence )
        , ( "operator", string inf.operator )
        ]


encodeDestructuring : Destructuring -> Value
encodeDestructuring { pattern, expression } =
    object
        [ ( "pattern", encodePattern pattern )
        , ( "expression", encodeExpression expression )
        ]


encodeType : Type -> Value
encodeType { name, generics, constructors } =
    object
        [ (nameField name)
        , ( "generics", asList string generics )
        , ( "constructors", asList encodeValueConstructor constructors )
        ]


encodeValueConstructor : ValueConstructor -> Value
encodeValueConstructor { name, arguments, range } =
    object
        [ (nameField name)
        , ( "arguments", asList encodeTypeReference arguments )
        , (rangeField range)
        ]


encodeTypeAlias : TypeAlias -> Value
encodeTypeAlias { name, generics, typeReference, range } =
    object
        [ (nameField name)
        , ( "generics", asList string generics )
        , ( "typeReference", encodeTypeReference typeReference )
        , (rangeField range)
        ]


encodeFunction : Function -> Value
encodeFunction { documentation, signature, declaration } =
    object
        [ ( "documentation", Maybe.map string documentation |> Maybe.withDefault JE.null )
        , ( "signature", Maybe.map encodeSignature signature |> Maybe.withDefault JE.null )
        , ( "declaration", encodeFunctionDeclaration declaration )
        ]


encodeSignature : FunctionSignature -> Value
encodeSignature { operatorDefinition, name, typeReference } =
    object
        [ ( "operatorDefinition", JE.bool operatorDefinition )
        , (nameField name)
        , ( "typeReference", encodeTypeReference typeReference )
        ]


encodeTypeReference : TypeReference -> Value
encodeTypeReference typeReference =
    case typeReference of
        GenericType name ->
            encodeTyped "generic" (string name)

        Typed moduleName name args ->
            encodeTyped "typed" <|
                object
                    [ ( "moduleName", encodeModuleName moduleName )
                    , (nameField name)
                    , ( "args", asList encodeTypeArg args )
                    ]

        Unit ->
            encodeTyped "unit" (JE.null)

        Tupled t ->
            encodeTyped "tupled" (asList encodeTypeReference t)

        FunctionTypeReference left right ->
            encodeTyped "function" <|
                object
                    [ ( "left", encodeTypeReference left )
                    , ( "right", encodeTypeReference right )
                    ]

        Record recordDefinition ->
            encodeTyped "record" (encodeRecordDefinition recordDefinition)

        GenericRecord name recordDefinition ->
            encodeTyped "genericRecord" <|
                object
                    [ (nameField name)
                    , ( "values", encodeRecordDefinition recordDefinition )
                    ]


encodeRecordDefinition : RecordDefinition -> Value
encodeRecordDefinition =
    list << List.map encodeRecordField


encodeRecordField : RecordField -> Value
encodeRecordField ( name, ref ) =
    object
        [ (nameField name)
        , ( "typeReference", encodeTypeReference ref )
        ]


encodeTypeArg : TypeArg -> Value
encodeTypeArg typeArg =
    case typeArg of
        Generic name ->
            encodeTyped "generic" (string name)

        Concrete tr ->
            encodeTyped "concrete" (encodeTypeReference tr)


encodeFunctionDeclaration : FunctionDeclaration -> Value
encodeFunctionDeclaration { operatorDefinition, name, arguments, expression } =
    object
        [ ( "operatorDefinition", JE.bool operatorDefinition )
        , ( "name", encodeVariablePointer name )
        , ( "arguments", asList encodePattern arguments )
        , ( "expression", encodeExpression expression )
        ]


encodeVariablePointer : VariablePointer -> Value
encodeVariablePointer { value, range } =
    object
        [ ( "value", string value )
        , (rangeField range)
        ]


encodePattern : Pattern -> Value
encodePattern pattern =
    case pattern of
        AllPattern ->
            encodeTyped "all" (JE.null)

        UnitPattern ->
            encodeTyped "unit" (JE.null)

        CharPattern c ->
            encodeTyped "char" (string <| String.fromChar c)

        StringPattern v ->
            encodeTyped "char" (string v)

        IntPattern i ->
            encodeTyped "int" (int i)

        FloatPattern f ->
            encodeTyped "float" (float f)

        TuplePattern patterns ->
            encodeTyped "tuple" (asList encodePattern patterns)

        RecordPattern pointers ->
            encodeTyped "record" (asList encodeVariablePointer pointers)

        UnConsPattern p1 p2 ->
            encodeTyped "uncons"
                (object
                    [ ( "left", encodePattern p1 )
                    , ( "right", encodePattern p2 )
                    ]
                )

        ListPattern patterns ->
            encodeTyped "list" (asList encodePattern patterns)

        VarPattern pointer ->
            encodeTyped "var" (encodeVariablePointer pointer)

        NamedPattern qualifiedNameRef patterns ->
            encodeTyped "named" <|
                object
                    [ ( "qualified", encodeQualifiedNameRef qualifiedNameRef )
                    , ( "patterns", asList encodePattern patterns )
                    ]

        QualifiedNamePattern qualifiedNameRef ->
            encodeTyped "qualifiedName" <| encodeQualifiedNameRef qualifiedNameRef

        AsPattern destructured variablePointer ->
            encodeTyped "as" <|
                object
                    [ ( "name", encodeVariablePointer variablePointer )
                    , ( "pattern", encodePattern destructured )
                    ]

        ParentisizedPattern p1 ->
            encodeTyped "parentisized" (encodePattern p1)


encodeQualifiedNameRef : QualifiedNameRef -> Value
encodeQualifiedNameRef { moduleName, name, range } =
    object
        [ ( "moduleName", encodeModuleName moduleName )
        , (nameField name)
        , (rangeField range)
        ]


encodeExpression : Expression -> Value
encodeExpression ( range, inner ) =
    object
        [ (rangeField range)
        , ( "inner"
          , case inner of
                UnitExpr ->
                    encodeTyped "unit" JE.null

                Application l ->
                    encodeTyped "application" (asList encodeExpression l)

                OperatorApplicationExpression operatorApplication ->
                    encodeTyped "operatorapplication" (encodeOperatorApplication operatorApplication)

                FunctionOrValue x ->
                    encodeTyped "functionOrValue" (string x)

                IfBlock c t e ->
                    encodeTyped "ifBlock" <|
                        object
                            [ ( "clause", encodeExpression c )
                            , ( "then", encodeExpression t )
                            , ( "else", encodeExpression e )
                            ]

                PrefixOperator x ->
                    encodeTyped "prefixoperator" (string x)

                Operator x ->
                    encodeTyped "operator" (string x)

                Integer x ->
                    encodeTyped "integer" (int x)

                Floatable x ->
                    encodeTyped "float" (float x)

                Literal x ->
                    encodeTyped "literal" (string x)

                CharLiteral c ->
                    encodeTyped "charLiteral" (string <| String.fromChar c)

                TupledExpression xs ->
                    encodeTyped "tupled" (asList encodeExpression xs)

                ListExpr xs ->
                    encodeTyped "list" (asList encodeExpression xs)

                ParenthesizedExpression x ->
                    encodeTyped "parenthesized" (encodeExpression x)

                LetExpression x ->
                    encodeTyped "let" <| encodeLetBlock x

                CaseExpression x ->
                    encodeTyped "case" <| encodeCaseBlock x

                LambdaExpression x ->
                    encodeTyped "lambda" <| encodeLambda x

                QualifiedExpr moduleName name ->
                    encodeTyped "qualified" <|
                        object
                            [ ( "moduleName", encodeModuleName moduleName )
                            , (nameField name)
                            ]

                RecordAccess exp name ->
                    encodeTyped "recordAccess" <|
                        object
                            [ ( "expression", encodeExpression exp )
                            , (nameField name)
                            ]

                RecordAccessFunction x ->
                    encodeTyped "recordAccessFunction" (string x)

                RecordExpr xs ->
                    encodeTyped "record" (asList encodeRecordSetter xs)

                RecordUpdateExpression recordUpdate ->
                    encodeTyped "recordUpdate" (encodeRecordUpdate recordUpdate)

                GLSLExpression x ->
                    encodeTyped "glsl" (string x)
          )
        ]


encodeRecordUpdate : RecordUpdate -> Value
encodeRecordUpdate { name, updates } =
    object
        [ (nameField name)
        , ( "updates", asList encodeRecordSetter updates )
        ]


encodeRecordSetter : RecordSetter -> Value
encodeRecordSetter ( field, expression ) =
    object
        [ ( "field", string field )
        , ( "expression", encodeExpression expression )
        ]


encodeLambda : Lambda -> Value
encodeLambda { args, expression } =
    object
        [ ( "patterns", asList encodePattern args )
        , ( "expression", encodeExpression expression )
        ]


encodeCaseBlock : CaseBlock -> Value
encodeCaseBlock { cases, expression } =
    object
        [ ( "cases", asList encodeCase cases )
        , ( "expression", encodeExpression expression )
        ]


encodeCase : Case -> Value
encodeCase ( pattern, expression ) =
    object
        [ ( "pattern", encodePattern pattern )
        , ( "expression", encodeExpression expression )
        ]


encodeLetBlock : LetBlock -> Value
encodeLetBlock { declarations, expression } =
    object
        [ ( "declarations", asList encodeDeclaration declarations )
        , ( "expression", encodeExpression expression )
        ]


encodeOperatorApplication : OperatorApplication -> Value
encodeOperatorApplication { operator, direction, left, right } =
    object
        [ ( "operator", string operator )
        , ( "direction", encodeInfixDirection direction )
        , ( "left", encodeExpression left )
        , ( "right", encodeExpression right )
        ]


encodeInfixDirection : InfixDirection -> Value
encodeInfixDirection d =
    case d of
        Left ->
            string "left"

        Right ->
            string "right"