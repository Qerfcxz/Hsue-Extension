{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Type where

import Engine.Type
import qualified Error.Type as ET
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP

data Slider_state=Slider_normal|Slider_hovered|Slider_pressed deriving Eq

data Extension_state a=Custom_extension_state {custom::Custom_extension_state a}

data Extension_event a=Custom_extension_event {custom::Custom_extension_event a}

data Extension_visual a=Page {window_id::Int,arrange::Arrange,x::FCT.CFloat,y::FCT.CFloat,half_width::FCT.CFloat,half_height::FCT.CFloat,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,step_size::FCT.CFloat,dirty::Bool,hovered::Bool,pressed::Bool,color::Color,outer_color::Color,hovered_color::Color,outer_hovered_color::Color,selected_color::Color,outer_selected_color::Color,hovered_selected_color::Color,outer_hovered_selected_color::Color,text::Visual a}|Button {window_id::Int,arrange::Arrange,x::FCT.CFloat,y::FCT.CFloat,half_width::FCT.CFloat,half_height::FCT.CFloat,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,dirty::Bool,hovered::Bool,pressed::Bool,color::Color,outer_color::Color,hovered_color::Color,outer_hovered_color::Color,pressed_color::Color,outer_pressed_color::Color,hovered_pressed_color::Color,outer_hovered_pressed_color::Color,text::Visual a}|Slider {window_id::Int,leaf_id::Int,selector::Selector (),getter::Widget a->(FCT.CFloat,FCT.CFloat,FCT.CFloat),setter::FCT.CFloat->FCT.CFloat->Maybe (Widget a->Widget a),arrange::Arrange,x::FCT.CFloat,y::FCT.CFloat,half_width::FCT.CFloat,half_height::FCT.CFloat,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,min_thumb_length::FCT.CFloat,step_size::FCT.CFloat,thumb_length::FCT.CFloat,thumb_position::FCT.CFloat,drag_position::FCT.CFloat,drag_offset::FCT.CFloat,cached_offset::FCT.CFloat,horizontal::Bool,dirty::Bool,thumb_state::Slider_state,first_triangle_state::Slider_state,second_triangle_state::Slider_state,color::Color,outer_color::Color,triangle_color::Color,triangle_hovered_color::Color,triangle_pressed_color::Color,thumb_color::Color,thumb_hovered_color::Color,thumb_pressed_color::Color}|Custom_extension_visual {custom::Custom_extension_visual a}

data Extension_visual_request a=Page_request {window_id::Int,arrange::Arrange,x::FCT.CFloat,y::FCT.CFloat,page_width::FCT.CFloat,page_height::FCT.CFloat,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,step_size::FCT.CFloat,calculate_width::DS.Seq Row->DS.Seq (DS.Seq Row)->Int->FCT.CFloat,calculate_typesetting::DS.Seq (DS.Seq Row)->Int->Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat),anchor::Anchor,article::DS.Seq (DS.Seq Sentence),load::Bool,color::Color,outer_color::Color,hovered_color::Color,outer_hovered_color::Color,selected_color::Color,outer_selected_color::Color,hovered_selected_color::Color,outer_hovered_selected_color::Color}|Button_request {window_id::Int,arrange::Arrange,x::FCT.CFloat,y::FCT.CFloat,button_width::FCT.CFloat,button_height::FCT.CFloat,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,anchor::Anchor,article::DS.Seq (DS.Seq Sentence),load::Bool,color::Color,outer_color::Color,hovered_color::Color,outer_hovered_color::Color,pressed_color::Color,outer_pressed_color::Color,hovered_pressed_color::Color,outer_hovered_pressed_color::Color}|Slider_request {window_id::Int,leaf_id::Int,selector::Selector (),getter::Widget a->(FCT.CFloat,FCT.CFloat,FCT.CFloat),setter::FCT.CFloat->FCT.CFloat->Maybe (Widget a->Widget a),arrange::Arrange,x::FCT.CFloat,y::FCT.CFloat,slider_width::FCT.CFloat,slider_height::FCT.CFloat,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,min_thumb_length::FCT.CFloat,step_size::FCT.CFloat,horizontal::Bool,color::Color,outer_color::Color,triangle_color::Color,triangle_hovered_color::Color,triangle_pressed_color::Color,thumb_color::Color,thumb_hovered_color::Color,thumb_pressed_color::Color}|Custom_extension_visual_request {custom::Custom_extension_visual_request a}

data Extension_submit_data a=Custom_extension_submit_data {custom::Custom_extension_submit_data a}

class (Custom a,Custom_state a~Extension_state a,Custom_event a~Extension_event a,Custom_visual a~Extension_visual a,Custom_visual_request a~Extension_visual_request a,Custom_submit_data a~Extension_submit_data a)=>Custom_extension a where
    type Custom_extension_state a=b|b->a
    type Custom_extension_event a=b|b->a
    type Custom_extension_visual a=b|b->a
    type Custom_extension_visual_request a=b|b->a
    type Custom_extension_submit_data a=b|b->a
    extension_custom_visual_collect::ET.Has_call_stack=>(Arrange->Arrange)->FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Custom_extension_visual a->DS.Seq (Submit a)
    extension_custom_visual_remove::ET.Has_call_stack=>Custom_extension_visual a->Engine a->IO (Engine a)
    extension_custom_visual_unlock::ET.Has_call_stack=>Custom_extension_visual a->Engine a->IO (Engine a,Custom_extension_visual a)
    extension_custom_visual_lock::ET.Has_call_stack=>Custom_extension_visual a->Custom_extension_visual a
    extension_custom_visual_request::ET.Has_call_stack=>Custom_extension_visual_request a->Engine a->IO (Engine a,Custom_extension_visual a)
    extension_custom_submit_data::ET.Has_call_stack=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->Custom_extension_submit_data a->IO ()