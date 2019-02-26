package translator


abstract class Token (token:String)

case class SoyParamToken(val token: String ="@param") extends Token(token)

case class ifToken(val id:String="if")
object SoyToken {

  val if_token = "{if"
  val end_if_token = "{/if}"
  val elseif_start_token = "{elseif"
  val else_token = "{else}"

  val namespace_token = "{namespace"

  val template_token = "{template"
  val template_private_attr_token = "private"
  val template_kind_attr_token = "kind"
  val template_stricthtml_attr_token = "stricthtml"
  val template_token_end = "{/template}"

  val param_declaration_token = "{@param"
  val param_declaration_optional_token = "{@param?"
  val param_end_token = "{/param}"
  val param_call_start="{param"

  val space_token = "{sp}"
  val nil_token = "{nil}" //empty string
  val carriage_return_token = "{\r}"
  val new_line_token = "{\n}"
  val tab_token = "{\t}"
  val left_brace_token = "{lb}"
  val right_brace_token = "{rb}"

  val print_start_token = "{$"
  val print_start_tradicional_token = "{print"

  val call_start_token = "{call"
  val call_end_token="{/call}"

  val alias_start_token = "{alias"
  val alias_as_token = "as"

  val literal_token = "{literal}"
  val literal_end_token = "{/literal}"

  val msg_start_token = "{msg"
  val msg_desc_attr_token = "desc"
  val msg_meaning_attr_token = "meaning"
  val msg_end_token = "{/msg}"
  val fallbackmsg_start_token = "{fallbackmsg"

  val var_declaration_start_token = "{let"
  val var_declaration_end_token = "{/let}"
  val variable_start_token = "$"

  val and_logic_token = "and"
  val not_logic_token = "not"
  val or_logic_token = "or"

  val token_end_slash = "/}"
  val token_end = "}"

  val switch_start_token = "{switch"
  val switch_end_token = "{/switch}"
  val switch_case_token = "{case"
  val switch_default_token = "{default}"

  val foreach_start_token="{foreach"
  val foreach_in_token="in"
  val if_empty_token="{ifempty}"
  val foreach_end_token="{/foreach}"

  val for_start_token="{for"
  val for_end_token="{/for}"
}
