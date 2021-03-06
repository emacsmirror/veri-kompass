[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/veri-kompass-badge.svg)](https://melpa.org/#/veri-kompass)

# veri-kompass

GNU Emacs extension that parse a Verilog design and provide some navigation facilities like hierarchy bar and wire following capabilities.
veri-kompass parse has a built-in parser, because of that does not depends on external software.

[![asciicast](https://asciinema.org/a/191880.png)](https://asciinema.org/a/191880)

## Installation

Clone this repo somewhere

Add into your .emacs

(add-to-list 'load-path "path-to-veri-kompass-here")
(require 'veri-kompass)
;; Enable veri kompass minor mode mode
(add-hook 'verilog-mode-hook 'veri-kompass-minor-mode)

## Usage

To start using veri-kompass:

- M-x veri-kompass
- Provide your folder directory
- Select your top module

Once in the hieracky bar select modules with RET to mark and visit them

In verilog sources follow signals as follow:

- C-c d to search for the drivers of the symbol at point
- C-c l to search for the loads of the symbol at point
