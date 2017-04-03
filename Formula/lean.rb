class Lean < Formula
  desc "Interactive, automatable, dependently-typed theorem prover"
  homepage "https://leanprover.github.io"
  url "https://github.com/leanprover/lean.git"
  version "0.2.0.20160602210703.gitc73b2860d5211187e9aa1039d1a49dcabdca4292"

  bottle do
    root_url "https://dl.bintray.com/lean/lean"
    sha256 "329689bfef46678547dca02ccc637ed7cd37f6831e0577aa75dc8af0c3231345" => :yosemite
    sha256 "ac9fe7e4f2c1ea33dc6af5dada8c662728f350cad459c69cd833980a92d93fb1" => :el_capitan
  end

  option "with-boost", "Compile using boost"

  # Required
  depends_on "gmp"
  depends_on "mpfr"
  depends_on "lua"
  depends_on "ninja"
  depends_on "cmake" => :build
  depends_on "boost" => [:build, :optional]

  def install
    args = ["-DCMAKE_INSTALL_PREFIX=#{prefix}",
            "-DCMAKE_BUILD_TYPE=Release",
            "-DTCMALLOC=OFF",
            "-GNinja",
            "-DLIBRARY_DIR=./"]
    args << "-DBOOST=ON" if build.with? "boost"
    mkdir "build" do
      system "curl", "-O", "-L", "https://github.com/leanprover/emacs-dependencies/archive/master.zip"
      system "unzip", "master.zip"
      mv "emacs-dependencies-master", "../src/emacs/dependencies"
      rm "master.zip"
      system "cmake", "../src", *args
      system "ninja", "clean"
      system "ninja"
      system "ninja", "install"
    end
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

  test do
    system "curl", "-O", "-L", "https://github.com/leanprover/lean/archive/master.zip"
    system "unzip", "master.zip"
    system "lean-master/tests/lean/test.sh", "#{bin}/lean"
    system "lean-master/tests/lua/test.sh",  "#{bin}/lean"
  end
end
