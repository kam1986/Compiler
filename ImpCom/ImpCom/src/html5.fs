module HTML5

open System

type htmlnode =
    | Text  of string
    | P     of htmlnode list
    | H1    of htmlnode list
    | H2    of htmlnode list
    | H3    of htmlnode list
    | H4    of htmlnode list
    | H5    of htmlnode list
    | H6    of htmlnode list
    | A     of Uri
    | Abbr  of htmlnode list

