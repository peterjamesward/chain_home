module ConfigPanel exposing (..)

{-	Allow user to dynamically select sets of targets.
	A UI front end to our Config module.
-}

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick)

import Target exposing (Target)
import Config exposing (TargetSelector, targetConfigurations)


-- UPDATE

type Msg
    = ToggleGroup groupID

-- We must store the active configurations in the model
init : Msg -> Model -> List TargetSelector
	targetConfigurations

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleGroup groupID ->
            { model | targets = toggleGroup model groupID }

toggleGroup model groupID =
	List.map (\ts -> if ts.id == groupID then 
						{ ts | active = !ts.active } 
					else 
						ts
		)
		model.targetConfigurations

-- VIEW

-- Map over the groups to create the UI
view : Model -> Html Msg
view model =
    fieldset []
        [ checkbox (ToggleGroup groupID) "...group description here..."
		...
        ]


checkbox : msg -> String -> Html msg
checkbox msg name =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]