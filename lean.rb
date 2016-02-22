require "formula"

class Lean < Formula
  homepage "http://leanprover.github.io"
  url "https://github.com/leanprover/lean.git"
  version "0.2.0.20160222161928.git431ce6ff2dc6ce98f5ba49753835f84dc9aebe31"

  bottle do
    root_url 'https://dl.bintray.com/lean/lean'
    sha256 'c7b9f11cb9aaf0cad87bf8a7087d93d302ed40c0953c3e665d47514130ea95ca' => :yosemite
    sha256 '213660b97342a9a9908501a330a451c11c3b10f0ff0b3c963eeb6f78441d0ba0' => :el_capitan
  end

  # Required
  depends_on 'gmp'
  depends_on 'mpfr'
  depends_on 'lua'
  depends_on 'ninja'
  depends_on 'cmake'            => :build
  option     "with-boost", "Compile using boost"
  depends_on 'boost'            => [:build, :optional]

  def install
    args = ["-DCMAKE_INSTALL_PREFIX=#{prefix}",
            "-DCMAKE_BUILD_TYPE=Release",
            "-DTCMALLOC=OFF",
            "-GNinja",
            "-DLIBRARY_DIR=./"]
    args << "-DBOOST=ON" if build.with? "boost"
    mkdir 'build' do
      system "curl", "-O", "-L", "https://github.com/leanprover/emacs-dependencies/archive/master.zip"
      system "unzip", "master.zip"
      system "mv", "emacs-dependencies-master", "../src/emacs/dependencies"
      system "rm", "master.zip"
      system "cmake", "../src", *args
      system "ninja"
      system "ninja", "install"
    end
  end

  test do
    system "curl", "-O", "-L", "https://github.com/leanprover/lean/archive/master.zip"
    system "unzip", "master.zip"
    system "lean-master/tests/lean/test.sh", "#{bin}/lean"
    system "lean-master/tests/lua/test.sh",  "#{bin}/lean"
  end

  def caveats; <<-EOS.undent
    Lean's Emacs mode is installed into
      #{HOMEBREW_PREFIX}/share/emacs/site-lisp/lean

    To use the Lean Emacs mode, you need to put the following lines in
    your .emacs file:
      (require 'package)
      (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (when (< emacs-major-version 24)
        (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
      (package-initialize)

      ;; Install required/optional packages for lean-mode
      (defvar lean-mode-required-packages
        '(company dash dash-functional flycheck f
                  fill-column-indicator s lua-mode mmm-mode))
      (let ((need-to-refresh t))
        (dolist (p lean-mode-required-packages)
          (when (not (package-installed-p p))
            (when need-to-refresh
              (package-refresh-contents)
              (setq need-to-refresh nil))
            (package-install p))))

      ;; Set up lean-root path
      (setq lean-rootdir "/usr/local")
      (setq-local lean-emacs-path "#{HOMEBREW_PREFIX}/share/emacs/site-lisp/lean")
      (add-to-list 'load-path (expand-file-name lean-emacs-path))
      (require 'lean-mode)
    EOS
  end
end
