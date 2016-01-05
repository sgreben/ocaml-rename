type t = { file : string; line : int; column : int; }

open Location
open Lexing

let right_of_location {file;line;column} location =
    location.pos_fname = file &&
    location.pos_lnum <= line &&
    location.pos_cnum - location.pos_bol <= column

let left_of_location {file;line;column} location =
    location.pos_fname = file &&
    location.pos_lnum >= line &&
    location.pos_cnum - location.pos_bol >= column