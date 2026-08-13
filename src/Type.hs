{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Type where

import Engine.Type
import qualified Foreign.C.Types as FCT

data Color=Color {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Extension_widget_request a b c d e=Page {window_id::Int,arrange::Arrange,visual_request::Visual_request,step_size::FCT.CFloat,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,inner_color::Color,outer_color::Color,inner_hovered_color::Color,outer_hovered_color::Color,inner_selected_color::Color,outer_selected_color::Color,inner_hovered_selected_color::Color,outer_hovered_selected_color::Color}|Button {window_id::Int,arrange::Arrange,visual_request::Visual_request,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,inner_color::Color,outer_color::Color,inner_hovered_color::Color,outer_hovered_color::Color,inner_pressed_color::Color,outer_pressed_color::Color,inner_hovered_pressed_color::Color,outer_hovered_pressed_color::Color}|Slider {window_id::Int,arrange::Arrange,leaf_id::Int,selector::Selector (),getter::Widget a b c d e->(FCT.CFloat,FCT.CFloat,FCT.CFloat),setter::FCT.CFloat->FCT.CFloat->Maybe (Widget a b c d e->Widget a b c d e),x::FCT.CFloat,y::FCT.CFloat,width::FCT.CFloat,height::FCT.CFloat,step_size::FCT.CFloat,min_thumb_length::FCT.CFloat,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,inner_color::Color,outer_color::Color,triangle_color::Color,triangle_hovered_color::Color,triangle_pressed_color::Color,thumb_color::Color,thumb_hovered_color::Color,thumb_pressed_color::Color,horizontal::Bool}

data Tag=Page_tag|Button_tag|Slider_tag