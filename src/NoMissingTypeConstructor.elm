module NoMissingTypeConstructor exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| -}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoMissingTypeConstructor" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { customTypes : Dict ModuleName (Dict String (Set String))
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , customTypes : Dict ModuleName (Dict String (Set String))
    }


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor


initialProjectContext : ProjectContext
initialProjectContext =
    { customTypes = Dict.empty
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { customTypes =
                moduleContext.customTypes
                    |> Dict.get []
                    |> Maybe.withDefault Dict.empty
                    |> Dict.singleton (Rule.moduleNameFromMetadata metadata)
            }
        )
        |> Rule.withMetadata


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            , customTypes = projectContext.customTypes
            }
        )
        |> Rule.withModuleNameLookupTable


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { customTypes = Dict.union newContext.customTypes previousContext.customTypes
    }



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor declarations context =
    -- Here we wish to find the custom types that were defined in the module, and store them in the context.
    ( []
    , { context
        | customTypes =
            Dict.insert []
                (declarations
                    |> List.filterMap getCustomType
                    |> List.map (\type_ -> ( Node.value type_.name, typeConstructors type_ ))
                    |> Dict.fromList
                )
                context.customTypes
      }
    )


typeConstructors : Type -> Set String
typeConstructors type_ =
    type_.constructors
        |> List.map (Node.value >> .name >> Node.value)
        |> Set.fromList


getCustomType : Node Declaration -> Maybe Type
getCustomType node =
    case Node.value node of
        Declaration.CustomTypeDeclaration type_ ->
            Just type_

        _ ->
            Nothing



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationVisitor declaration context =
    {- Here, we are interested in the declarations of the form
       allXyz : List Xyz
       allXyz = [ ... ]
    -}
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            let
                functionName : Node String
                functionName =
                    getFunctionName function
            in
            if String.startsWith "all" (Node.value functionName) then
                case getTypeAnnotation function |> Maybe.andThen (getListOfTypeAnnotation context.lookupTable) of
                    Just ( moduleName, typeName ) ->
                        -- At this point, we established we are in the definition
                        -- of a definition like the one mentioned above.
                        case Dict.get moduleName context.customTypes |> Maybe.andThen (Dict.get typeName) of
                            Just constructors ->
                                let
                                    usedConstructors : Set String
                                    usedConstructors =
                                        function.declaration
                                            |> Node.value
                                            |> .expression
                                            |> availableConstructors

                                    missingConstructors : Set String
                                    missingConstructors =
                                        Set.diff constructors usedConstructors
                                in
                                if Set.isEmpty missingConstructors then
                                    ( []
                                    , context
                                    )

                                else
                                    ( [ Rule.error
                                            { message = "`" ++ Node.value functionName ++ "` does not contain all the type constructors for `" ++ typeName ++ "`"
                                            , details =
                                                [ "We expect `" ++ Node.value functionName ++ "` to contain all the type constructors for `" ++ typeName ++ "`."
                                                , """In this case, you are missing the following constructors:
    , """
                                                    ++ (missingConstructors |> Set.toList |> String.join "\n    , ")
                                                ]
                                            }
                                            (Node.range functionName)
                                      ]
                                    , context
                                    )

                            Nothing ->
                                ( []
                                , context
                                )

                    Nothing ->
                        ( [], context )

            else
                ( [], context )

        _ ->
            ( [], context )


availableConstructors : Node Expression -> Set String
availableConstructors expr =
    case Node.value expr of
        Expression.ListExpr list ->
            list
                |> List.filterMap constructorName
                |> Set.fromList

        _ ->
            Set.empty


constructorName : Node Expression -> Maybe String
constructorName expr =
    case Node.value expr of
        Expression.FunctionOrValue _ name ->
            Just name

        _ ->
            Nothing


getFunctionName : Expression.Function -> Node String
getFunctionName function =
    function.declaration
        |> Node.value
        |> .name


getTypeAnnotation : Expression.Function -> Maybe TypeAnnotation
getTypeAnnotation function =
    function.signature
        |> Maybe.map (Node.value >> .typeAnnotation >> Node.value)


getListOfTypeAnnotation : ModuleNameLookupTable -> TypeAnnotation -> Maybe ( ModuleName, String )
getListOfTypeAnnotation lookupTable typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.Typed typeNode (parameterNode :: []) ->
            case ( Node.value typeNode, Node.value parameterNode ) of
                ( ( [], "List" ), TypeAnnotation.Typed parameter _ ) ->
                    case ModuleNameLookupTable.moduleNameFor lookupTable parameter of
                        Just moduleName ->
                            Just ( moduleName, Node.value parameter |> Tuple.second )

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
