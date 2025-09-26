" Vim syntax file
" Language: Scheme (R7RS)
" Last Change: 2021-01-03
" Author: Evan Hanson <evhan@foldling.org>
" Maintainer: Evan Hanson <evhan@foldling.org>
" Previous Author: Dirk van Deun <dirk@igwe.vub.ac.be>
" Previous Maintainer: Sergey Khorev <sergey.khorev@gmail.com>
" Repository: https://git.foldling.org/vim-scheme.git
" URL: https://foldling.org/vim/syntax/scheme.vim

if exists('b:current_syntax')
  finish
endif

let s:cpo = &cpo
set cpo&vim

syn spell notoplevel

syn match schemeParentheses "[^ '`\t\n()\[\]";]\+"
syn match schemeParentheses "[)\]]"

syn match schemeIdentifier /[^ '`\t\n()\[\]"|;][^ '`\t\n()\[\]"|;]*/

syn region schemeQuote matchgroup=schemeData start=/'[`']*/ end=/[ \t\n()\[\]}";]/me=e-1
syn region schemeQuote matchgroup=schemeData start=/'['`]*"/ skip=/\\[\\"]/ end=/"/
syn region schemeQuote matchgroup=schemeData start=/'['`]*|/ skip=/\\[\\|]/ end=/|/
syn region schemeQuote matchgroup=schemeData start=/'['`]*#\?(/ end=/)/ contains=ALLBUT,schemeQuasiquote,schemeQuasiquoteForm,schemeUnquote,schemeForm,schemeDatumCommentForm,schemeImport,@schemeImportCluster,@schemeSyntaxCluster

syn region schemeQuasiquote matchgroup=schemeData start=/`['`]*/ end=/[ \t\n()\[\]";]/me=e-1
syn region schemeQuasiquote matchgroup=schemeData start=/`['`]*#\?(/ end=/)/ contains=ALLBUT,schemeQuote,schemeQuoteForm,schemeForm,schemeDatumCommentForm,schemeImport,@schemeImportCluster,@schemeSyntaxCluster

syn region schemeUnquote matchgroup=schemeParentheses start=/,/ end=/[ `'\t\n\[\]()";]/me=e-1 contained contains=ALLBUT,schemeDatumCommentForm,@schemeImportCluster
syn region schemeUnquote matchgroup=schemeParentheses start=/,@/ end=/[ `'\t\n\[\]()";]/me=e-1 contained contains=ALLBUT,schemeDatumCommentForm,@schemeImportCluster
syn region schemeUnquote matchgroup=schemeParentheses start=/,(/ end=/)/ contained contains=ALLBUT,schemeDatumCommentForm,@schemeImportCluster
syn region schemeUnquote matchgroup=schemeParentheses start=/,@(/ end=/)/ contained contains=ALLBUT,schemeDatumCommentForm,@schemeImportCluster

syn region schemeQuoteForm matchgroup=schemeData start=/(/ end=/)/ contained contains=ALLBUT,schemeQuasiquote,schemeQuasiquoteForm,schemeUnquote,schemeForm,schemeDatumCommentForm,schemeImport,@schemeImportCluster,@schemeSyntaxCluster
syn region schemeQuasiquoteForm matchgroup=schemeData start=/(/ end=/)/ contained contains=ALLBUT,schemeQuote,schemeForm,schemeDatumCommentForm,schemeImport,@schemeImportCluster,@schemeSyntaxCluster

syn region schemeString start=/\(\\\)\@<!"/ skip=/\\[\\"]/ end=/"/ contains=@Spell
syn region schemeSymbol start=/\(\\\)\@<!|/ skip=/\\[\\|]/ end=/|/

syn match schemeNumber /\(#[dbeio]\)*[+\-]*\([0-9]\+\|inf.0\|nan.0\)\(\/\|\.\)\?[0-9+\-@\ilns]*\>/
syn match schemeNumber /#x[+\-]*[0-9a-fA-F]\+\>/

syn match schemeBoolean /#t\(rue\)\?/
syn match schemeBoolean /#f\(alse\)\?/

syn match schemeCharacter /#\\.[^ `'\t\n\[\]()]*/
syn match schemeCharacter /#\\x[0-9a-fA-F]\+/

syn match schemeComment /;.*$/ contains=@Spell

syn region schemeMultilineComment start=/#|/ end=/|#/ contains=schemeMultilineComment,@Spell

syn region schemeForm matchgroup=schemeParentheses start="(" end=")" contains=ALLBUT,schemeUnquote,schemeDatumCommentForm,@schemeImportCluster
syn region schemeForm matchgroup=schemeParentheses start="\[" end="\]" contains=ALLBUT,schemeUnquote,schemeDatumCommentForm,@schemeImportCluster

syn region schemeVector matchgroup=schemeData start="#(" end=")" contains=ALLBUT,schemeQuasiquote,schemeQuasiquoteForm,schemeUnquote,schemeForm,schemeDatumCommentForm,schemeImport,@schemeImportCluster,@schemeSyntaxCluster
syn region schemeVector matchgroup=schemeData start="#[fsu]\d\+(" end=")" contains=schemeNumber,schemeComment,schemeDatumComment

if exists('g:is_chicken') || exists('b:is_chicken')
  syn region schemeImport matchgroup=schemeImport start="\(([ \t\n]*\)\@<=\(import\|import-syntax\|use\|require-extension\)\(-for-syntax\)\?\>" end=")"me=e-1 contained contains=schemeImportForm,schemeIdentifier,schemeComment,schemeDatumComment
else
  syn region schemeImport matchgroup=schemeImport start="\(([ \t\n]*\)\@<=\(import-from\)\>" end=")"me=e-1 contained contains=schemeImportForm,schemeIdentifier,schemeComment,schemeDatumComment
endif

syn match   schemeImportKeyword "\(([ \t\n]*\)\@<=\(except\|only\|prefix\|rename\)\>"
syn region  schemeImportForm matchgroup=schemeParentheses start="(" end=")" contained contains=schemeIdentifier,schemeComment,schemeDatumComment,@schemeImportCluster
syn cluster schemeImportCluster contains=schemeImportForm,schemeImportKeyword

syn region schemeDatumComment matchgroup=schemeDatumComment start=/#;[ \t\n`']*/ end=/[ \t\n()\[\]";]/me=e-1
syn region schemeDatumComment matchgroup=schemeDatumComment start=/#;[ \t\n`']*"/ skip=/\\[\\"]/ end=/"/
syn region schemeDatumComment matchgroup=schemeDatumComment start=/#;[ \t\n`']*|/ skip=/\\[\\|]/ end=/|/
syn region schemeDatumComment matchgroup=schemeDatumComment start=/#;[ \t\n`']*\(#\([usf]\d\+\)\?\)\?(/ end=/)/ contains=schemeDatumCommentForm
syn region schemeDatumCommentForm start="(" end=")" contained contains=schemeDatumCommentForm

syn cluster schemeSyntaxCluster contains=schemeFunction,schemeKeyword,schemeSyntax,schemeExtraSyntax,schemeLibrarySyntax,schemeSyntaxSyntax

syn keyword schemeSyntaxSyntax define-syntax

syn keyword schemeSyntax and
syn keyword schemeSyntax or
syn keyword schemeSyntax do
syn keyword schemeSyntax define
syn keyword schemeSyntax define-macro
syn keyword schemeSyntax assign
syn keyword schemeSyntax assert
syn keyword schemeSyntax do
syn keyword schemeSyntax if
syn keyword schemeSyntax fn
syn keyword schemeSyntax let
syn keyword schemeSyntax quote
syn keyword schemeSyntax kv-quote
syn keyword schemeSyntax =
syn keyword schemeSyntax &
syn keyword schemeSyntax #key
syn keyword schemeSyntax #either
syn keyword schemeSyntax #default
syn keyword schemeSyntax nil
syn keyword schemeSyntax #rec
syn keyword schemeSyntax #clear-args:rec

syn keyword schemeFunction boolean?
syn keyword schemeFunction procedure?
syn keyword schemeFunction number?
syn keyword schemeFunction integer?
syn keyword schemeFunction string?
syn keyword schemeFunction list?
syn keyword schemeFunction kv?
syn keyword schemeFunction not
syn keyword schemeFunction nil?
syn keyword schemeFunction const
syn keyword schemeFunction identity
syn keyword schemeFunction empty?
syn keyword schemeFunction kv-set
syn keyword schemeFunction push
syn keyword schemeFunction push-back
syn keyword schemeFunction head
syn keyword schemeFunction first
syn keyword schemeFunction second
syn keyword schemeFunction tail
syn keyword schemeFunction last
syn keyword schemeFunction but-last
syn keyword schemeFunction all?
syn keyword schemeFunction any?
syn keyword schemeFunction fold-right
syn keyword schemeFunction all-sequential-values?
syn keyword schemeFunction list:==
syn keyword schemeFunction list:!=
syn keyword schemeFunction list:+
syn keyword schemeFunction list:*
syn keyword schemeFunction +
syn keyword schemeFunction *
syn keyword schemeFunction ==
syn keyword schemeFunction !=
syn keyword schemeFunction length
syn keyword schemeFunction list:<
syn keyword schemeFunction list:<=
syn keyword schemeFunction list:>
syn keyword schemeFunction list:>=
syn keyword schemeFunction <
syn keyword schemeFunction <=
syn keyword schemeFunction >
syn keyword schemeFunction >=
syn keyword schemeFunction reverse
syn keyword schemeFunction apply-if
syn keyword schemeFunction replace-if
syn keyword schemeFunction fold-left
syn keyword schemeFunction list:-
syn keyword schemeFunction list:/
syn keyword schemeFunction -
syn keyword schemeFunction /
syn keyword schemeFunction map
syn keyword schemeFunction filter
syn keyword schemeFunction display!
syn keyword schemeFunction print!
syn keyword schemeFunction compose
syn keyword schemeFunction o
syn keyword schemeFunction modulo
syn keyword schemeFunction item
syn keyword schemeFunction take
syn keyword schemeFunction drop
syn keyword schemeFunction take-while
syn keyword schemeFunction drop-while
syn keyword schemeFunction pipe
syn keyword schemeFunction cont-push
syn keyword schemeFunction list:append
syn keyword schemeFunction append
syn keyword schemeFunction one-of?
syn keyword schemeFunction kv-from-list
syn keyword schemeFunction modify-item
syn keyword schemeFunction split
syn keyword schemeFunction modify-item*
syn keyword schemeFunction replace-item
syn keyword schemeFunction replace-item*
syn keyword schemeFunction partition
syn keyword schemeFunction sort

syn keyword schemeFunction read-file-string!


hi def link schemeBoolean Boolean
hi def link schemeCharacter Character
hi def link schemeComment Comment
hi def link schemeConstant Constant
hi def link schemeData Delimiter
hi def link schemeDatumComment Comment
hi def link schemeDatumCommentForm Comment
hi def link schemeDelimiter Delimiter
hi def link schemeError Error
hi def link schemeExtraSyntax Underlined
hi def link schemeFunction Function
hi def link schemeIdentifier Normal
hi def link schemeImport PreProc
hi def link schemeImportKeyword PreProc
hi def link schemeKeyword Type
hi def link schemeLibrarySyntax PreProc
hi def link schemeMultilineComment Comment
hi def link schemeNumber Number
hi def link schemeParentheses Normal
hi def link schemeQuasiquote Delimiter
hi def link schemeQuote Delimiter
hi def link schemeSpecialSyntax Special
hi def link schemeString String
hi def link schemeSymbol Normal
hi def link schemeSyntax Statement
hi def link schemeSyntaxSyntax PreProc
hi def link schemeTypeSyntax Type

let b:did_scheme_syntax = 1

if exists('b:is_chicken') || exists('g:is_chicken')
  exe 'ru! syntax/chicken.vim'
endif

unlet b:did_scheme_syntax
let b:current_syntax = 'scheme'
let &cpo = s:cpo
unlet s:cpo
