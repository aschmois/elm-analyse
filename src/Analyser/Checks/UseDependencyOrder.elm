module Analyser.Checks.UseDependencyOrder exposing (checker)

import AST.Ranges as Ranges
import ASTUtil.Inspector as Inspector exposing (Order(..), defaultConfig)
import Analyser.Checks.Base exposing (Checker)
import Analyser.Configuration exposing (Configuration)
import Analyser.FileContext exposing (FileContext)
import Analyser.Messages.Data as Data exposing (MessageData)
import Analyser.Messages.Schema as Schema
import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))


type alias Context =
    { typeDecls : Dict String Range
    , valDecls : Dict String Range
    , msgs : List MessageData
    }


initContext : FileContext -> Context
initContext fileContext =
    let
        add (Node range name) =
            Dict.insert name range

        proc (Node _ decl) =
            case decl of
                FunctionDeclaration f ->
                    Tuple.mapSecond (add (Node.value f.declaration).name)

                AliasDeclaration t ->
                    case Node.value t.typeAnnotation of
                        Record _ ->
                            Tuple.mapBoth (add t.name) (add t.name)

                        _ ->
                            Tuple.mapFirst (add t.name)

                CustomTypeDeclaration t ->
                    Tuple.mapBoth
                        (add t.name)
                        (List.foldl (\(Node _ vc) -> (<<) (add vc.name)) identity t.constructors)

                _ ->
                    identity

        ( typeDecls, valDecls ) =
            List.foldl proc ( Dict.empty, Dict.empty ) fileContext.ast.declarations
    in
    { typeDecls = typeDecls, valDecls = valDecls, msgs = [] }


withMessage : String -> String -> Range -> Context -> Context
withMessage label name range context =
    let
        msg =
            Data.init
                (String.concat
                    [ label ++ " `"
                    , name
                    , "` used before its definition at "
                    , Ranges.rangeToString range
                    ]
                )
                |> Data.addVarName "varName" name
                |> Data.addRange "range" range
    in
    { context | msgs = msg :: context.msgs }


onTypeAnnotation : Node TypeAnnotation -> Context -> Context
onTypeAnnotation (Node range ann) context =
    let
        update =
            case ann of
                Typed (Node _ ( [], name )) _ ->
                    case Dict.get name context.typeDecls of
                        Just defRange ->
                            if range.start.row < defRange.start.row then
                                withMessage "Type" name range

                            else
                                identity

                        Nothing ->
                            identity

                _ ->
                    identity
    in
    update context


onExpression : Node Expression -> Context -> Context
onExpression (Node range expr) context =
    let
        update =
            case expr of
                FunctionOrValue [] name ->
                    case Dict.get name context.valDecls of
                        Just loc ->
                            if range.start.row < loc.start.row then
                                withMessage "Value" name range

                            else
                                identity

                        _ ->
                            identity

                _ ->
                    identity
    in
    update context


scan : FileContext -> Configuration -> List MessageData
scan fileContext _ =
    Inspector.inspect
        { defaultConfig
            | onTypeAnnotation = Post onTypeAnnotation
            , onExpression = Post onExpression
        }
        fileContext.ast
        (initContext fileContext)
        |> .msgs


checker : Checker
checker =
    { check = scan
    , info =
        { key = "UseDependencyOrder"
        , name = "Use Dependency Postorder"
        , description = "TODO"
        , schema =
            Schema.schema
                |> Schema.varProp "varName"
                |> Schema.rangeProp "range"
        }
    }
