{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Button where

import Engine.Collector
import Engine.Container
import Engine.Operation
import Engine.Projection
import Engine.Selector
import Engine.Type
import Engine.Underlying
import qualified Error.Error as EE
import qualified Data.Functor.Compose as DFC
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Foreign.C.Types as FCT

button_widget_trigger::(Engine a b c d e->Engine a b c d e)->Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)
button_widget_trigger this_action event _ widget=case event of
    At {window_id,action}->case widget of
        Vector {vector_widget}->if window_id==get_store_widget (vector_widget DV.! 4) then case action of
            Click {press,mouse_button,x,y}->case mouse_button of
                Mouse_button_left->case press of
                    Press_up->if view_button_bool widget 2 then let new_widget=update_button_bool (const True) 3 (update_button_bool (const False) 2 widget) in if above_button x y (vector_widget DV.! 0) then (new_widget,this_action) else (new_widget,id) else (widget,id)
                    Press_down->if above_button x y (vector_widget DV.! 0) then (update_button_bool (const True) 3 (update_button_bool (const True) 2 widget),id) else (widget,id)
                _->(widget,id)
            Move {x,y}->let above=above_button x y (vector_widget DV.! 0) in if above/=view_button_bool widget 1 then (update_button_bool (const True) 3 (update_button_bool (const above) 1 widget),if above then \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}}) else (widget,id)
            _->(widget,id)
        else (widget,id)
        _->EE.quick_error "button_widget_trigger" 0
    _->(widget,id)

above_button::FCT.CFloat->FCT.CFloat->Widget a b c d e->Bool
above_button x y widget=case widget of
    Vector_visual {arrange=first_arrange,vector_visual}->case vector_visual DV.! 1 of
        Rectangle {arrange=second_arrange,half_width,half_height}->case combine_arrange first_arrange second_arrange of
            Arrange {point,matrix}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=x-point.x-matrix.x in let new_y=y-point.y-matrix.y in abs (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant)<=half_width&&abs (matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)<=half_height
        _->EE.quick_error "above_button" 0
    _->EE.quick_error "above_button" 1

view_button_bool::Widget a b c d e->Int->Bool
view_button_bool widget index=case widget of
    Vector {vector_widget}->case vector_widget DV.! index of
        Store {store}->convert store
        _->EE.quick_error "view_button_bool" 0
    _->EE.quick_error "view_button_bool" 1

view_button::Widget a b c d e->Widget a b c d e
view_button this_widget=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->case vector_widget DV.! 0 of
            Vector_visual {arrange,size,vector_visual}->let hovered=get_store_widget (vector_widget DV.! 1) in let pressed=get_store_widget (vector_widget DV.! 2) in let offset=if pressed then if hovered then 6 else 4 else if hovered then 2 else 0 in Vector_visual {arrange=arrange,collect_order=(2+offset) DS.<| (1+offset) DS.<| DS.singleton 0,size=size,vector_visual=vector_visual}
            _->EE.quick_error "view_button" 0
        _->EE.quick_error "view_button" 1
    _->EE.quick_error "view_button" 2

update_button::Widget a b c d e->Maybe (Widget a b c d e)
update_button this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->if get_store_widget (vector_widget DV.! 3) then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_widget 3 (update_store_widget (const False)) widget}) else Nothing
        _->EE.quick_error "update_button" 0
    _->EE.quick_error "update_button" 1

update_button_bool::(Bool->Bool)->Int->Widget a b c d e->Widget a b c d e
update_button_bool update index=update_vector_widget index (update_store_widget update)

maybe_update_collect_button::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_update_collect_button maybe_border projection_path leaf_id selector collect_strategy engine=case DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect engine.u engine.v maybe_border (view_button this_widget),this_widget)) (selector_monad_update (const update_button) selector widget)}) engine) of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=intmap_update leaf_id (update_projection_object (collect_a submit collect_strategy)) new_engine.leaf}

maybe_collect_update_button::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_collect_update_button maybe_border projection_path leaf_id selector collect_strategy engine=let (update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(intmap_update leaf_id (update_projection_object (collect_a (to_collect engine.u engine.v maybe_border (view_button widget)) collect_strategy)),selector_monad_update (const update_button) selector widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=update new_engine.leaf}

collect_button::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
collect_button maybe_border projection_path leaf_id selector collect_strategy engine=engine {leaf=intmap_update leaf_id (update_projection_object (selector_update (const (collect_a (to_collect engine.u engine.v maybe_border (view_button (lookup_projection_widget projection_path engine))) collect_strategy)) selector)) engine.leaf}

{-# INLINE view_button_bool #-}
{-# INLINE update_button_bool #-}