{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Page where

import Common
import Type
import Engine.Collector
import Engine.Container
import Engine.Operation
import Engine.Projection
import Engine.Selector
import Engine.Text
import Engine.Type
import Engine.Underlying
import qualified Error.Error as EE
import qualified Data.Functor.Compose as DFC
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Foreign.C.Types as FCT

extension_page_visual_index::Int
extension_page_visual_index=0

extension_page_hovered_index::Int
extension_page_hovered_index=1

extension_page_selected_index::Int
extension_page_selected_index=2

extension_page_dirty_index::Int
extension_page_dirty_index=3

extension_page_window_id_index::Int
extension_page_window_id_index=4

extension_page_text_visual_index::Int
extension_page_text_visual_index=0

extension_page_inner_rectangle_index::Int
extension_page_inner_rectangle_index=1

update_text::(Visual->Visual)->Visual->(Visual,Bool)
update_text update visual=case visual of
    Text {current_y=first_y}->let new_visual=update visual in case new_visual of
        Text {current_y=second_y}->(new_visual,first_y/=second_y)
        _->EE.quick_error "update_text" 0
    _->EE.quick_error "update_text" 1

scroll_page::(Visual->Visual)->Widget a b c d e->Widget a b c d e
scroll_page transform widget=case widget of
    Vector {vector_widget}->case vector_widget DV.! extension_page_visual_index of
        Vector_visual {arrange,collect_order,size=visual_size,vector_visual}->let (new_text_visual,changed)=update_text transform (vector_visual DV.! extension_page_text_visual_index) in if changed then update_vector_widget extension_page_dirty_index (update_store_widget (const True)) (update_vector_widget extension_page_visual_index (const (Vector_visual {arrange=arrange,collect_order=collect_order,size=visual_size,vector_visual=DV.modify (\this_vector_widget->DVM.write this_vector_widget extension_page_text_visual_index new_text_visual) vector_visual})) widget) else widget
        _->EE.quick_error "scroll_page" 0
    _->EE.quick_error "scroll_page" 1

create_page_request::(Event a->Engine b a c d e->Maybe Int)->(Event a->Engine b a c d e->Widget b a c d e->(Widget b a c d e,Engine b a c d e->Engine b a c d e))->Extension_widget_request b a c d e->Widget_request b a c d e
create_page_request next widget_trigger page_request=case page_request of
    Page {window_id,arrange,visual_request,inner_thickness,outer_thickness,inner_color,outer_color,inner_hovered_color,outer_hovered_color,inner_selected_color,outer_selected_color,inner_hovered_selected_color,outer_hovered_selected_color}->case visual_request of
        Text_request {text_width,text_height}->let center_x=arrange.point.x in let center_y=arrange.point.y in let inner_width=text_width/2+inner_thickness in let inner_height=text_height/2+inner_thickness in let outer_width=inner_width+outer_thickness in let outer_height=inner_height+outer_thickness in Widget_trigger_request {next=next,widget_trigger=widget_trigger,widget_request=Vector_request {index=0,vector_widget_request=DS.singleton (Vector_visual_request {arrange=arrange,collect_order=2 DS.<| 1 DS.<| DS.singleton extension_page_text_visual_index,vector_visual_request=DV.fromList [visual_request,create_rectangle_request center_x center_y inner_color inner_width inner_height,create_rectangle_request center_x center_y outer_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_color outer_width outer_height,create_rectangle_request center_x center_y inner_selected_color inner_width inner_height,create_rectangle_request center_x center_y outer_selected_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_selected_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_selected_color outer_width outer_height]}) DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert True} DS.|> Store_request {store=convert window_id}}}
        _->EE.quick_error "create_page_request" 0
    _->EE.quick_error "create_page_request" 1

page_widget_trigger::FCT.CFloat->Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)
page_widget_trigger step_size event _ widget=case event of
    At {window_id,action}->case widget of
        Vector {vector_widget}->if window_id==get_store_widget (vector_widget DV.! extension_page_window_id_index) then case action of
            Move {x,y}->let above=above_page x y (vector_widget DV.! extension_page_visual_index) in let hovered=view_page_bool widget extension_page_hovered_index in if above/=hovered then (update_vector_widget extension_page_dirty_index (update_store_widget (const True)) (update_vector_widget extension_page_hovered_index (update_store_widget (const above)) widget),if above then \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}}) else (widget,id)
            Click {press,mouse_button,x=x,y=y}->case mouse_button of
                Mouse_button_left->case press of
                    Press_down->let above=above_page x y (vector_widget DV.! extension_page_visual_index) in let selected=view_page_bool widget extension_page_selected_index in if above/=selected then (update_vector_widget extension_page_dirty_index (update_store_widget (const True)) (update_vector_widget extension_page_selected_index (update_store_widget (const above)) widget),id) else (widget,id)
                    _->(widget,id)
                _->(widget,id)
            Scroll {x,y,delta_y}->if above_page x y (vector_widget DV.! extension_page_visual_index) then (scroll_page (scroll_text (negate delta_y*step_size)) widget,id) else (widget,id)
            Press {press,change}->case press of
                Press_down->if view_page_bool widget extension_page_selected_index then case change of
                    Key_down->(scroll_page (scroll_text step_size) widget,id)
                    Key_up->(scroll_page (scroll_text (negate step_size)) widget,id)
                    Key_page_down->(scroll_page scroll_bottom_text widget,id)
                    Key_page_up->(scroll_page scroll_top_text widget,id)
                    _->(widget,id)
                else (widget,id)
                _->(widget,id)
            _->(widget,id)
        else (widget,id)
        _->(widget,id)
    _->(widget,id)

above_page::FCT.CFloat->FCT.CFloat->Widget a b c d e->Bool
above_page x y widget=case widget of
    Vector_visual {arrange=first_arrange,vector_visual}->case vector_visual DV.! extension_page_inner_rectangle_index of
        Rectangle {arrange=second_arrange,half_width,half_height}->case combine_arrange first_arrange second_arrange of
            Arrange {point,matrix}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=x-point.x-matrix.x in let new_y=y-point.y-matrix.y in abs (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant)<=half_width&&abs (matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)<=half_height
        _->EE.quick_error "above_page" 0
    _->EE.quick_error "above_page" 1

view_page_bool::Widget a b c d e->Int->Bool
view_page_bool widget index=case widget of
    Vector {vector_widget}->case vector_widget DV.! index of
        Store {store}->convert store
        _->EE.quick_error "view_page_bool" 0
    _->EE.quick_error "view_page_bool" 1

update_page_bool::(Bool->Bool)->Int->Widget a b c d e->Widget a b c d e
update_page_bool update index=update_vector_widget index (update_store_widget update)

view_page::Widget a b c d e->Widget a b c d e
view_page this_widget=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->case vector_widget DV.! extension_page_visual_index of
            Vector_visual {arrange,size,vector_visual}->let hovered=get_store_widget (vector_widget DV.! extension_page_hovered_index) in let selected=get_store_widget (vector_widget DV.! extension_page_selected_index) in let offset=if selected then if hovered then 6 else 4 else if hovered then 2 else 0 in Vector_visual {arrange=arrange,collect_order=(2+offset) DS.<| (1+offset) DS.<| DS.singleton extension_page_text_visual_index,size=size,vector_visual=vector_visual}
            _->EE.quick_error "view_page" 0
        _->EE.quick_error "view_page" 1
    _->EE.quick_error "view_page" 2

update_page::Widget a b c d e->Maybe (Widget a b c d e)
update_page this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->if get_store_widget (vector_widget DV.! extension_page_dirty_index) then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_page_bool (const False) extension_page_dirty_index widget}) else Nothing
        _->EE.quick_error "update_page" 0
    _->EE.quick_error "update_page" 1

maybe_update_collect_page::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_update_collect_page maybe_border projection_path leaf_id selector collect_strategy engine=case DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect engine.u engine.v maybe_border (view_page this_widget),this_widget)) (selector_monad_update (const update_page) selector widget)}) engine) of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=intmap_update leaf_id (update_projection_object (collect_a submit collect_strategy)) new_engine.leaf}

maybe_collect_update_page::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_collect_update_page maybe_border projection_path leaf_id selector collect_strategy engine=let (update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(intmap_update leaf_id (update_projection_object (collect_a (to_collect engine.u engine.v maybe_border (view_page widget)) collect_strategy)),selector_monad_update (const update_page) selector widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=update new_engine.leaf}

collect_page::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
collect_page maybe_border projection_path leaf_id selector collect_strategy engine=engine {leaf=intmap_update leaf_id (update_projection_object (selector_update (const (collect_a (to_collect engine.u engine.v maybe_border (view_page (lookup_projection_widget projection_path engine))) collect_strategy)) selector)) engine.leaf}

{-# INLINE extension_page_visual_index #-}
{-# INLINE extension_page_hovered_index #-}
{-# INLINE extension_page_selected_index #-}
{-# INLINE extension_page_dirty_index #-}
{-# INLINE extension_page_window_id_index #-}
{-# INLINE extension_page_text_visual_index #-}
{-# INLINE extension_page_inner_rectangle_index #-}
{-# INLINE update_text #-}
{-# INLINE scroll_page #-}
{-# INLINE above_page #-}
{-# INLINE view_page_bool #-}
{-# INLINE update_page_bool #-}
{-# INLINE view_page #-}
{-# INLINE update_page #-}
{-# INLINE maybe_update_collect_page #-}
{-# INLINE maybe_collect_update_page #-}
{-# INLINE collect_page #-}