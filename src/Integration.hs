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
import Engine.Type
import qualified Error.Error as EE
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Foreign.C.Types as FCT

create_extension_widget_request::(Event a->Engine b a c d e->Maybe Int)->(Engine b a c d e->Engine b a c d e)->Extension_widget_request b a c d e->Widget_request b a c d e
create_extension_widget_request next action extension_widget_request=case extension_widget_request of
    Page {step_size}->create_page_request next (page_widget_trigger step_size) extension_widget_request
    Button {window_id,arrange,visual_request,inner_thickness,outer_thickness,inner_color,outer_color,inner_hovered_color,outer_hovered_color,inner_pressed_color,outer_pressed_color,inner_hovered_pressed_color,outer_hovered_pressed_color}->case visual_request of
        Text_request {text_width,text_height}->let center_x=arrange.point.x in let center_y=arrange.point.y in let inner_width=text_width/2+inner_thickness in let inner_height=text_height/2+inner_thickness in let outer_width=inner_width+outer_thickness in let outer_height=inner_height+outer_thickness in Widget_trigger_request {next=next,widget_trigger=button_widget_trigger action,widget_request=Vector_request {index=extension_button_root_vector_index,vector_widget_request=DS.singleton (Vector_visual_request {arrange=arrange,collect_order=extension_button_outer_rectangle_base_offset DS.<| extension_button_inner_rectangle_base_offset DS.<| DS.singleton extension_button_content_visual_index,vector_visual_request=DV.fromList [visual_request,create_rectangle_request center_x center_y inner_color inner_width inner_height,create_rectangle_request center_x center_y outer_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_color outer_width outer_height,create_rectangle_request center_x center_y inner_pressed_color inner_width inner_height,create_rectangle_request center_x center_y outer_pressed_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_pressed_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_pressed_color outer_width outer_height]}) DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert True} DS.|> Store_request {store=convert window_id}}}
        _->EE.empty_error
    Slider {window_id,arrange,leaf_id,selector,getter,setter,x,y,width,height,step_size,min_thumb_length,inner_thickness,outer_thickness,inner_color,outer_color,triangle_color,triangle_hovered_color,triangle_pressed_color,thumb_color,thumb_hovered_color,thumb_pressed_color,horizontal}->let center_x=arrange.point.x+x in let center_y=arrange.point.y+y in let half_width=width/2 in let half_height=height/2 in let inner_half_width=half_width-outer_thickness in let inner_half_height=half_height-outer_thickness in let radius=if horizontal then inner_half_height-inner_thickness else inner_half_width-inner_thickness in let first_triangle_center_x=if horizontal then center_x-inner_half_width+2*inner_thickness+radius else center_x in let first_triangle_center_y=if horizontal then center_y else center_y+inner_half_height-2*inner_thickness-radius in let second_triangle_center_x=if horizontal then center_x+inner_half_width-2*inner_thickness-radius else center_x in let second_triangle_center_y=if horizontal then center_y else center_y-inner_half_height+2*inner_thickness+radius in let first_triangle_first_point=if horizontal then Point {x=negate radius,y=0} else Point {x=0,y=radius} in let first_triangle_second_point=if horizontal then Point {x=radius,y=radius} else Point {x=negate radius,y=negate radius} in let first_triangle_third_point=Point {x=radius,y=negate radius} in let second_triangle_first_point=if horizontal then Point {x=radius,y=0} else Point {x=0,y=negate radius} in let second_triangle_second_point=Point {x=negate radius,y=radius} in let second_triangle_third_point=if horizontal then Point {x=negate radius,y=negate radius} else Point {x=radius,y=radius} in let thumb_base_half_width=if horizontal then 1 else radius in let thumb_base_half_height=if horizontal then radius else 1 in let track_start_position=if horizontal then center_x-inner_half_width+3*inner_thickness+2*radius else center_y+inner_half_height-3*inner_thickness-2*radius in let track_end_position=if horizontal then center_x+inner_half_width-3*inner_thickness-2*radius else center_y-inner_half_height+3*inner_thickness+2*radius in Widget_trigger_request {next=next,widget_trigger=slider_widget_trigger leaf_id selector getter setter center_x center_y radius track_start_position track_end_position step_size first_triangle_center_x first_triangle_center_y second_triangle_center_x second_triangle_center_y horizontal,widget_request=Vector_request {index=0,vector_widget_request=DS.fromList [Vector_visual_request {arrange=arrange {point=Point {x=center_x,y=center_y}},collect_order=DS.fromList [extension_slider_outer_rectangle_visual_index,extension_slider_inner_rectangle_visual_index,extension_slider_first_triangle_visual_base_index,extension_slider_second_triangle_visual_base_index,extension_slider_thumb_visual_base_index],vector_visual_request=DV.fromList [create_rectangle_request center_x center_y outer_color half_width half_height,create_rectangle_request center_x center_y inner_color inner_half_width inner_half_height,create_triangle_request first_triangle_center_x first_triangle_center_y triangle_color first_triangle_first_point first_triangle_second_point first_triangle_third_point,create_triangle_request first_triangle_center_x first_triangle_center_y triangle_hovered_color first_triangle_first_point first_triangle_second_point first_triangle_third_point,create_triangle_request first_triangle_center_x first_triangle_center_y triangle_pressed_color first_triangle_first_point first_triangle_second_point first_triangle_third_point,create_triangle_request second_triangle_center_x second_triangle_center_y triangle_color second_triangle_first_point second_triangle_second_point second_triangle_third_point,create_triangle_request second_triangle_center_x second_triangle_center_y triangle_hovered_color second_triangle_first_point second_triangle_second_point second_triangle_third_point,create_triangle_request second_triangle_center_x second_triangle_center_y triangle_pressed_color second_triangle_first_point second_triangle_second_point second_triangle_third_point,create_rectangle_request center_x center_y thumb_color thumb_base_half_width thumb_base_half_height,create_rectangle_request center_x center_y thumb_hovered_color thumb_base_half_width thumb_base_half_height,create_rectangle_request center_x center_y thumb_pressed_color thumb_base_half_width thumb_base_half_height]},Store_request {store=convert window_id},Store_request {store=convert extension_slider_state_normal},Store_request {store=convert extension_slider_state_normal},Store_request {store=convert extension_slider_state_normal},Store_request {store=convert (0::FCT.CFloat)},Store_request {store=convert (0::FCT.CFloat)},Store_request {store=convert (0::FCT.CFloat)},Store_request {store=convert (abs (track_end_position-track_start_position))},Store_request {store=convert track_start_position},Store_request {store=convert (if horizontal then extension_slider_horizontal_flag_true else extension_slider_horizontal_flag_false::Int)},Store_request {store=convert min_thumb_length},Store_request {store=convert center_x},Store_request {store=convert center_y},Store_request {store=convert True}]}}

maybe_update_collect_extension_widget::Custom_widget e=>Tag->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_update_collect_extension_widget tag=case tag of
    Page_tag->maybe_update_collect update_page view_page
    Button_tag->maybe_update_collect update_button view_button
    Slider_tag->maybe_update_collect update_slider view_slider

maybe_collect_update_extension_widget::Custom_widget e=>Tag->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_collect_update_extension_widget tag=case tag of
    Page_tag->maybe_collect_update update_page view_page
    Button_tag->maybe_collect_update update_button view_button
    Slider_tag->maybe_collect_update update_slider view_slider

collect_extension_widget::Custom_widget e=>Tag->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
collect_extension_widget tag=case tag of
    Page_tag->collect view_page
    Button_tag->collect view_button
    Slider_tag->collect view_slider

{-# INLINE create_extension_widget_request #-}
{-# INLINE maybe_update_collect_extension_widget #-}
{-# INLINE maybe_collect_update_extension_widget #-}
{-# INLINE collect_extension_widget #-}