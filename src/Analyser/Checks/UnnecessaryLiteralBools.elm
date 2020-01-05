module Analyser.Checks.UnnecessaryLiteralBools exposing (checker)

import AST.Ranges as Range
import ASTUtil.Inspector as Inspector exposing (Order(..), defaultConfig)
import Analyser.Checks.Base exposing (Checker)
import Analyser.Configuration exposing (Configuration)
import Analyser.FileContext exposing (FileContext)
import Analyser.Messages.Data as Data exposing (MessageData)
import Analyser.Messages.Schema as Schema
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))


checker : Checker
checker =
    { check = scan
    , info =
        { key = "UnnecessaryLiteralBools"
        , name = "Unnecessary Literal Booleans"
        , description = "Directly use the boolean you already have."
        , schema =
            Schema.schema
                |> Schema.rangeProp "range"
        }
    }


type alias Context =
    List MessageData


scan : FileContext -> Configuration -> List MessageData
scan fileContext _ =
    Inspector.inspect
        { defaultConfig
            | onExpression = Post onExpression
        }
        fileContext.ast
        []


onExpression : Node Expression -> Context -> Context
onExpression (Node r expr) context =
    let
        withMsg msg =
            (Data.init
                (String.concat
                    [ msg
                    , " at "
                    , Range.rangeToString r
                    ]
                )
                |> Data.addRange "range" r
            )
                :: context

        getLiteralBool s =
            case s of
                FunctionOrValue _ "True" ->
                    Just True

                FunctionOrValue _ "False" ->
                    Just False

                _ ->
                    Nothing
    in
    case expr of
        IfBlock _ (Node _ left) (Node _ right) ->
            case ( getLiteralBool left, getLiteralBool right ) of
                ( Just True, Just True ) ->
                    withMsg "Replace if-block with `True`"

                ( Just True, Just False ) ->
                    withMsg "Replace if-block with just the if-condition"

                ( Just False, Just True ) ->
                    withMsg "Replace if-block with just the negation of the if-condition"

                ( Just False, Just False ) ->
                    withMsg "Replace if-block with `False`"

                ( Just True, Nothing ) ->
                    withMsg "Replace if-block with `<condition> || <else branch>`"

                ( Just False, Nothing ) ->
                    withMsg "Replace if-block with `not <condition> && <else branch>`"

                ( Nothing, Just True ) ->
                    withMsg "Replace if-block with `not <condition> || <then branch>`"

                ( Nothing, Just False ) ->
                    withMsg "Replace if-block with `<condition> && <then branch>`"

                ( Nothing, Nothing ) ->
                    context

        OperatorApplication name _ (Node _ left) (Node _ right) ->
            let
                isValidOp =
                    name == "==" || name == "/=" || name == "&&" || name == "||"

                hasBoolOperand =
                    getLiteralBool left /= Nothing || getLiteralBool right /= Nothing
            in
            if isValidOp && hasBoolOperand then
                withMsg "Simplify expression involving a literal Bool value"

            else
                context

        _ ->
            context
