{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Integration where

import Button
import Common
import Page
import Slider
import Type
import Engine.Collector
import Engine.Container
import Engine.Projection
import Engine.Selector
import Engine.Type
import Engine.Underlying
import qualified Data.Functor.Compose as DFC
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Error.Type as ET
import qualified Foreign.C.Types as FCT

create_extension_widget_request::ET.Has_call_stack=>(Event a->Engine a->Maybe Int)->(Engine a->Engine a)->Extension_widget_request a->Widget_request a
create_extension_widget_request next action extension_widget_request=case extension_widget_request of
    Page {}->create_page_request next extension_widget_request
    Button {}->create_button_request next action extension_widget_request
    Slider {}->create_slider_request next extension_widget_request

get_update::ET.Has_call_stack=>Tag->Widget a->Maybe (Widget a)
get_update tag=case tag of
    Page_tag->update_page
    Button_tag->update_button
    Slider_tag->update_slider

get_view::ET.Has_call_stack=>Tag->Widget a->Widget a
get_view tag=case tag of
    Page_tag->view_page
    Button_tag->view_button
    Slider_tag->view_slider

get_active_indices::ET.Has_call_stack=>Tag->Widget a->[Int]
get_active_indices tag widget=let vector_widget=extract_extension_widget_vector widget in case tag of
    Page_tag->let hovered=get_store_widget (vector_widget DV.! extension_hovered_index) in let state=get_store_widget (vector_widget DV.! extension_state_index) in let offset=if state then if hovered then extension_state_hovered_offset else extension_state_unhovered_offset else if hovered then extension_normal_hovered_offset else extension_normal_unhovered_offset in [extension_outer_base_offset+offset,extension_inner_base_offset+offset,extension_content_index]
    Button_tag->let hovered=get_store_widget (vector_widget DV.! extension_hovered_index) in let state=get_store_widget (vector_widget DV.! extension_state_index) in let offset=if state then if hovered then extension_state_hovered_offset else extension_state_unhovered_offset else if hovered then extension_normal_hovered_offset else extension_normal_unhovered_offset in [extension_outer_base_offset+offset,extension_inner_base_offset+offset,extension_content_index]
    Slider_tag->let thumb_state=get_store_widget (vector_widget DV.! extension_slider_thumb_state_index) in let first_triangle_state=get_store_widget (vector_widget DV.! extension_slider_first_triangle_state_index) in let second_triangle_state=get_store_widget (vector_widget DV.! extension_slider_second_triangle_state_index) in [extension_slider_outer_rectangle_visual_index,extension_slider_inner_rectangle_visual_index,extension_slider_first_triangle_visual_base_index+first_triangle_state,extension_slider_second_triangle_visual_base_index+second_triangle_state,extension_slider_thumb_visual_base_index+thumb_state]

transform_visual_selector::ET.Has_call_stack=>Tag->Widget a->Visual_selector b->Visual_selector b
transform_visual_selector tag widget sel=case sel of
    Any_visual_selector {value,strict}->Combine_visual_selector {combine_visual_selector=DS.fromList (map (\i->Vector_visual_selector {vector_value=DIM.singleton i value,bounded=True,strict=strict}) (get_active_indices tag widget))}
    Combine_visual_selector {combine_visual_selector}->Combine_visual_selector {combine_visual_selector=fmap (transform_visual_selector tag widget) combine_visual_selector}
    _->sel

maybe_update_collect_extension_widget::ET.Has_call_stack=>Custom c=>Tag->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Visual_selector b->Insert_strategy->Engine c->Engine c
maybe_update_collect_extension_widget tag maybe_border projection_path leaf_id selector visual_selector collect_strategy engine=let update=get_update tag in let view=get_view tag in case DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect (transform_visual_selector tag this_widget visual_selector) engine.u engine.v maybe_border (view this_widget),this_widget)) (selector_monad_update (const update) selector widget)}) engine) of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=int_map_update leaf_id (update_projection_object (collect_a submit collect_strategy)) new_engine.leaf}

maybe_collect_update_extension_widget::ET.Has_call_stack=>Custom c=>Tag->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Visual_selector b->Insert_strategy->Engine c->Engine c
maybe_collect_update_extension_widget tag maybe_border projection_path leaf_id selector visual_selector collect_strategy engine=let update=get_update tag in let view=get_view tag in let (new_update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(int_map_update leaf_id (update_projection_object (collect_a (to_collect (transform_visual_selector tag widget visual_selector) engine.u engine.v maybe_border (view widget)) collect_strategy)),selector_monad_update (const update) selector widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=new_update new_engine.leaf}

collect_extension_widget::ET.Has_call_stack=>Custom c=>Tag->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Visual_selector b->Insert_strategy->Engine c->Engine c
collect_extension_widget tag maybe_border projection_path leaf_id selector visual_selector collect_strategy engine=let widget=lookup_projection_widget projection_path engine in let view=get_view tag in let active_selector=transform_visual_selector tag widget visual_selector in engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const (collect_a (to_collect active_selector engine.u engine.v maybe_border (view widget)) collect_strategy)) selector)) engine.leaf}

{-# INLINE create_extension_widget_request #-}
{-# INLINE get_update #-}
{-# INLINE get_view #-}
{-# INLINE get_active_indices #-}
{-# INLINE transform_visual_selector #-}
{-# INLINE maybe_update_collect_extension_widget #-}
{-# INLINE maybe_collect_update_extension_widget #-}
{-# INLINE collect_extension_widget #-}