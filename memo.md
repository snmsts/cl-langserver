# もやもや
こういうもやもやしたものを、英語で考えをダンプでけん。

# 目的
lispの開発インフラを他で開発されるユーティリティから使いやすくする。

# 至る過程。

## swankへの不満
undocumentedかつクライアントにemacsと同じS式リーダを要求する。
slimeのバージョン変更で何かがかわったりすると追従できなくてそこで死んだりする。
結果CL好きはemacsにロックインされている。swankはそもそもそういう目的で作られているし
emacsがギルティーではないにしてもclツール開発をとりまく状況は宜しくない。近代化せねばな。

## そこで
language-server-protocolにswankのエッセンシャルな部分を追加する。
静的言語の類には必要性は薄いけれど、
swankの実装を外から見てopenなプロトコルにしていくのなら、
undocumentedな部分を全部仕様に興すよりは差分の方がはやかろうという考え。
swank/emacsの実際のやりとりを整理(reverse-engineering)して、
language-serverプロトコルに統合して要らなくなるもの、
必要な物に分類する作業が必要なのでやる。

知見は死なないはずなので記録に残す。

## 参考資料
language server protocol https://github.com/Microsoft/language-server-protocol/
swank https://github.com/slime/slime
swankプロトコルの解説 https://github.com/astine/swank-client/blob/master/swank-description.markdown

## swankプロトコルメッセージ一覧
from swank.lisp dispatch-event

sortしてない。出現順

- :arrow_right: :emacs-rex
- :arrow_left: :return
- :emacs-interrupt
- :arrow_left: :write-string
- :debug
- :debug-condition
- :debug-activate
- :debug-return
- :channel-send
- :presentation-start
- :presentation-end
- :new-package
- :new-features
- :ed
- :indentation-update
- :eval
- :eval-no-wait
- :background-message
- :inspect
- :ping
- :y-or-n-p
- :read-from-minibuffer
- :read-string
- :read-aborted
- :test-delay
- :write-image
- :emacs-pong
- :emacs-return
- :emacs-return-string
- :emacs-channel-send
- :reader-error

# emacs-rex

swankではemacs-rexがrpcとして使われている。
swankのソース上でdefslimefunで定義されている関数がemacs-rexのメッセージとして扱われているものと考える。
ちゃんと知識として網羅してないので、git grepしててごねしてからuniqしてsortした。
今回の要件としては時間が無いので全部確認するのではなく適宜トリアージュして解説する。

- create-mrepl

- add
- apropos-list-for-emacs
- apropos-list-for-emacssimple-completions
- asdf-determine-system
- asdf-system-directory
- asdf-system-files
- asdf-system-loaded-p
- autodoc
- background-save-snapshot
- backtrace
- buffer-first-change
- clear-repl-results
- clear-repl-variables
- clear-trace-tree
- commit-edited-value
- compile-file-for-emacs
- compile-file-if-needed
- compile-multiple-strings-for-emacs
- compile-string-for-emacs
- compiler-notes-for-emacs
- complete-form
- completions
- completions-for-character
- completions-for-keyword
- connection-info
- create-repl
- debug-nth-thread
- debugger-info-for-emacs
- delete-entry
- delete-system-fasls
- describe-definition-for-emacs
- describe-function
- describe-inspectee
- describe-symbol
- dialog-toggle-trace
- dialog-trace
- dialog-traced-p
- dialog-untrace
- dialog-untrace-all
- disassemble-form
- documentation-symbol
- ed-in-emacs
- entries
- entry-to-ref
- eval-and-grab-output
- eval-string-in-frame
- export-structure
- export-symbol-for-emacs
- filename-to-modulename
- find-definition-for-thing
- find-definitions-for-emacs
- find-source-location-for-emacs
- flow-control-test
- frame-locals-and-catch-tags
- frame-package-name
- frame-source-location
- fuzzy-completion-selected
- fuzzy-completions
- hyperdoc
- init-inspector
- init-presentation-streams
- init-presentations
- inspect-current-condition
- inspect-frame-var
- inspect-in-emacs
- inspect-in-frame
- inspect-nth-part
- inspect-presentation
- inspect-trace-part
- inspector-call-nth-action
- inspector-eval
- inspector-history
- inspector-next
- inspector-nth-part
- inspector-pop
- inspector-range
- inspector-reinspect
- inspector-toggle-verbose
- interactive-eval
- interactive-eval-region
- invoke-nth-restart
- invoke-nth-restart-for-emacs
- io-speed-test
- kill-nth-thread
- list-all-package-names
- list-all-systems-in-central-registry
- list-all-systems-known-to-asdf
- list-asdf-systems
- list-quicklisp-systems
- list-threads
- listener-eval
- listener-get-value
- listener-save-value
- load-file
- lookup-and-save-presented-object-or-lose
- lookup-presented-object
- lookup-presented-object-or-lose
- macro-form-p
- macrostep-expand-1
- mop
- operate-on-system-for-emacs
- operator-arglist
- package=
- ping
- pprint-eval
- pprint-eval-string-in-frame
- pprint-inspector-part
- profile-by-substring
- quit-inspector
- quit-lisp
- quit-thread-browser
- re-evaluate-defvar
- redirect-trace-output
- reload-system
- repl-eval-hook-pass
- repl-suppress-advance-history
- repl-suppress-output
- report-partial-tree
- report-specs
- report-total
- report-trace-detail
- restore-snapshot
- save-snapshot
- sdlb-print-condition
- set-default-directory
- set-package
- simple-break
- simple-completions
- sldb-abort
- sldb-break
- sldb-break-with-default-debugger
- sldb-continue
- sldb-disassemble
- sldb-return-from-frame
- start-swank-server-in-thread
- swank-compiler-macroexpand
- swank-compiler-macroexpand-1
- swank-delete-package
- swank-expand
- swank-expand-1
- swank-expand-all
- swank-format-string-expand
- swank-macroexpand
- swank-macroexpand-1
- swank-macroexpand-all
- swank-profile-package
- swank-require
- swank-sprof-disassemble
- swank-sprof-expand-node
- swank-sprof-get-call-graph
- swank-sprof-source-location
- swank-sprof-start
- swank-sprof-stop
- swank-toggle-trace
- throw-to-toplevel
- toggle-break-on-signals
- toggle-debug-on-swank-error
- toggle-profile-fdefinition
- undefine-function
- unexport-symbol-for-emacs
- unintern-symbol
- untrace-all
- update-indentation-information
- value-for-editing
- who-depends-on
- xref
- xrefs

