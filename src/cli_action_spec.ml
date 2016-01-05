type summary_length =
    | Sl_very_short
    | Sl_short
    | Sl_medium

type symbol_spec =
    | Ss_point of Point.t
    | Ss_longident of Longident.t

type action_desc =
    | Ad_rename_value
    | Ad_rename_type
    | Ad_rename_field

type action_mode =
    | Am_dry
    | Am_inplace
    | Am_inplace_backup
    | Am_inplace_backup_patch

type action_extra = {
    ae_mode : action_mode;
    ae_summary_length : summary_length;
}

type action = {
    a_desc : action_desc * symbol_spec * string;
    a_extra : action_extra;
}