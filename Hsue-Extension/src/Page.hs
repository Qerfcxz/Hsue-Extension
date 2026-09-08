{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Page where

import Type
import Underlying
import Engine.Collector
import Engine.Helper
import Engine.Text
import Engine.Type
import Engine.Underlying
import Engine.Widget
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.HashMap.Strict as DHMS
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT

apply_page_action::ET.Has_call_stack=>Bool->FCT.CFloat->Page_action->Visual a->Visual a
apply_page_action strict_match step_size page_action text=case page_action of
    Page_scroll_up->scroll_text strict_match (negate step_size) text
    Page_scroll_down->scroll_text strict_match step_size text
    Page_scroll_top->scroll_top_text strict_match text
    Page_scroll_bottom->scroll_bottom_text strict_match text

create_page_request::ET.Has_call_stack=>Custom_extension a=>(Event a->Engine a->Maybe Int)->Extension_visual_request a->Widget_request a
create_page_request next page_request=case page_request of
    Page_request {}->Visual_trigger_request {next=next,visual_trigger=page_visual_trigger,visual_request=Custom_visual_request {visual_request_custom=page_request}}
    _->EF.empty_error

create_page_visual::ET.Has_call_stack=>Custom_extension a=>Extension_visual_request a->Engine a->IO (Engine a,Extension_visual a)
create_page_visual page_request engine=case page_request of
    Page_request {window_id,arrange,x,y,page_width,page_height,inner_thickness,outer_thickness,step_size,failure_advance,failure_left,failure_down,failure_right,failure_up,max_search_index,calculate_width,calculate_typesetting,anchor,article,load,color,outer_color,hovered_color,outer_hovered_color,selected_color,outer_selected_color,hovered_selected_color,outer_hovered_selected_color,key_setting_request,strict_exist,strict_match}->do
        (new_engine,text_visual)<-create_visual (Text_request {arrange=default_arrange,text_width=max 0 (page_width-2*inner_thickness),text_height=max 0 (page_height-2*inner_thickness),failure_advance=failure_advance,failure_left=failure_left,failure_down=failure_down,failure_right=failure_right,failure_up=failure_up,max_search_index=max_search_index,calculate_width=calculate_width,calculate_typesetting=calculate_typesetting,anchor=anchor,article=article,load=load}) engine
        return (new_engine,Page {window_id=window_id,arrange=arrange,x=x,y=y,half_width=page_width/2,half_height=page_height/2,inner_thickness=inner_thickness,outer_thickness=outer_thickness,step_size=step_size,dirty=True,hovered=False,pressed=False,color=color,outer_color=outer_color,hovered_color=hovered_color,outer_hovered_color=outer_hovered_color,selected_color=selected_color,outer_selected_color=outer_selected_color,hovered_selected_color=hovered_selected_color,outer_hovered_selected_color=outer_hovered_selected_color,text=text_visual,key_setting_request=key_setting_request,key_setting=to_key_setting key_setting_request,strict_exist=strict_exist,strict_match=strict_match})
    _->EF.empty_error

get_page_color::ET.Has_call_stack=>Bool->Bool->Color->Color->Color->Color->Color
get_page_color hovered pressed normal hovered_color selected_color hovered_selected_color=if pressed then if hovered then hovered_selected_color else selected_color else if hovered then hovered_color else normal

update_page_state::ET.Has_call_stack=>Bool->Bool->Bool->Bool->Visual a->Extension_visual a->Extension_visual a
update_page_state strict_match dirty hovered pressed text page=case page of
    Page {}->page {dirty=dirty,hovered=hovered,pressed=pressed,text=text}
    _->if strict_match then EF.empty_error else page

page_visual_trigger::ET.Has_call_stack=>Custom_extension a=>Event a->Engine a->Visual a->(Visual a,Engine a->Engine a)
page_visual_trigger event _ visual=case event of
    At {window_id=this_window_id,action}->case visual of
        Custom_visual {visual_custom}->case visual_custom of
            Page {window_id,arrange,x,y,half_width,half_height,step_size,hovered,pressed,text,key_setting,strict_exist,strict_match}->if this_window_id==window_id
                then case action of
                    Press {press,maintain}->case press of
                        Press_down->if pressed
                            then case DHMS.lookup (from_foldable_enumeration maintain) key_setting of
                                Nothing->(visual,id)
                                Just page_action->(Custom_visual {visual_custom=update_page_state strict_match True hovered pressed (apply_page_action strict_match step_size page_action text) visual_custom},id)
                            else (visual,id)
                        Press_up->(visual,id)
                    Click {press,mouse_button,x=click_x,y=click_y}->case mouse_button of
                        Mouse_button_left->case press of
                            Press_down->let above=above_extension_box click_x click_y arrange x y half_width half_height in if above/=pressed then (Custom_visual {visual_custom=update_page_state strict_match True hovered above text visual_custom},id) else (visual,id)
                            Press_up->(visual,id)
                        Mouse_button_right->case press of
                            Press_down->let above=above_extension_box click_x click_y arrange x y half_width half_height in if above&&pressed then (Custom_visual {visual_custom=update_page_state strict_match True hovered False text visual_custom},id) else (visual,id)
                            Press_up->(visual,id)
                        _->(visual,id)
                    Move {x=move_x,y=move_y}->let above=above_extension_box move_x move_y arrange x y half_width half_height in if above/=hovered then (Custom_visual {visual_custom=update_page_state strict_match True above pressed text visual_custom},if above then \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer,strict_exist=strict_exist}} else \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default,strict_exist=strict_exist}}) else (visual,id)
                    Scroll {x=scroll_x,y=scroll_y,delta_y}->if pressed&&above_extension_box scroll_x scroll_y arrange x y half_width half_height then (Custom_visual {visual_custom=update_page_state strict_match True hovered pressed (scroll_text strict_match (negate delta_y*step_size) text) visual_custom},id) else (visual,id)
                    _->(visual,id)
                else (visual,id)
            _->(visual,id)
        _->(visual,id)
    _->(visual,id)

collect_page_visual::ET.Has_call_stack=>Custom_extension a=>(Arrange->Arrange)->FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Extension_visual a->DS.Seq (Submit a)
collect_page_visual transform u v maybe_border page=case page of
    Page {arrange,x,y,half_width,half_height,inner_thickness,outer_thickness,hovered,pressed,color,outer_color,hovered_color,outer_hovered_color,selected_color,outer_selected_color,hovered_selected_color,outer_hovered_selected_color,text}->case text of
        Text {arrange=text_arrange,current_y,anchor,article}->let page_arrange=combine_arrange (transform arrange) (default_arrange {point=Point {x=x,y=y}}) in case page_arrange of
            Arrange {point=page_point,matrix=page_matrix,color=page_base_color}->create_submit_rectangle Submit_default maybe_border (Arrange {point=page_point,matrix=page_matrix,color=multiply_color page_base_color (get_page_color hovered pressed outer_color outer_hovered_color outer_selected_color outer_hovered_selected_color)}) (half_width+outer_thickness) (half_height+outer_thickness) u v u v DS.<| create_submit_rectangle Submit_default maybe_border (Arrange {point=page_point,matrix=page_matrix,color=multiply_color page_base_color (get_page_color hovered pressed color hovered_color selected_color hovered_selected_color)}) half_width half_height u v u v DS.<| DS.singleton (create_submit_text Submit_default maybe_border (combine_arrange page_arrange text_arrange) (half_width-inner_thickness) (half_height-inner_thickness) current_y anchor article)
        _->EF.empty_error
    _->EF.empty_error

page_getter::ET.Has_call_stack=>Custom_extension a=>Widget a->(FCT.CFloat,FCT.CFloat,FCT.CFloat)
page_getter widget=case widget of
    Visual_trigger {visual}->case visual of
        Custom_visual {visual_custom}->case visual_custom of
            Page {text}->case text of
                Text {half_height,current_y,min_y,max_y}->(max_y-min_y+2*half_height,2*half_height,current_y-min_y)
                _->EF.empty_error
            _->EF.empty_error
        _->EF.empty_error
    _->EF.empty_error

page_setter::ET.Has_call_stack=>Custom_extension a=>Bool->FCT.CFloat->FCT.CFloat->Maybe (Widget a->Widget a)
page_setter strict_match cached_offset offset=if cached_offset==offset then Nothing else Just (page_setter_a strict_match offset)

page_setter_a::ET.Has_call_stack=>Custom_extension a=>Bool->FCT.CFloat->Widget a->Widget a
page_setter_a strict_match offset widget=case widget of
    Visual_trigger {next,visual_trigger,visual}->case visual of
        Custom_visual {visual_custom}->case visual_custom of
            Page {window_id,arrange,x,y,half_width,half_height,inner_thickness,outer_thickness,step_size,hovered,pressed,color,outer_color,hovered_color,outer_hovered_color,selected_color,outer_selected_color,hovered_selected_color,outer_hovered_selected_color,text,key_setting_request,key_setting,strict_exist,strict_match=this_strict_match}->case text of
                Text {arrange=text_arrange,half_width=text_half_width,half_height=text_half_height,failure_advance,failure_left,failure_down,failure_right,failure_up,min_y,max_y,anchor,article,charset,locked}->Visual_trigger {next=next,visual_trigger=visual_trigger,visual=Custom_visual {visual_custom=Page {window_id=window_id,arrange=arrange,x=x,y=y,half_width=half_width,half_height=half_height,inner_thickness=inner_thickness,outer_thickness=outer_thickness,step_size=step_size,dirty=True,hovered=hovered,pressed=pressed,color=color,outer_color=outer_color,hovered_color=hovered_color,outer_hovered_color=outer_hovered_color,selected_color=selected_color,outer_selected_color=outer_selected_color,hovered_selected_color=hovered_selected_color,outer_hovered_selected_color=outer_hovered_selected_color,text=Text {arrange=text_arrange,half_width=text_half_width,half_height=text_half_height,failure_advance=failure_advance,failure_left=failure_left,failure_down=failure_down,failure_right=failure_right,failure_up=failure_up,current_y=max min_y (min (max min_y max_y) (min_y+offset)),min_y=min_y,max_y=max_y,anchor=anchor,article=article,charset=charset,locked=locked},key_setting_request=key_setting_request,key_setting=key_setting,strict_exist=strict_exist,strict_match=this_strict_match}}}
                _->if strict_match then EF.empty_error else widget
            _->if strict_match then EF.empty_error else widget
        _->if strict_match then EF.empty_error else widget
    _->if strict_match then EF.empty_error else widget

{-# INLINE apply_page_action #-}
{-# INLINE create_page_request #-}
{-# INLINE create_page_visual #-}
{-# INLINE get_page_color #-}
{-# INLINE update_page_state #-}
{-# INLINE page_visual_trigger #-}
{-# INLINE collect_page_visual #-}
{-# INLINE page_getter #-}
{-# INLINE page_setter #-}
{-# INLINE page_setter_a #-}