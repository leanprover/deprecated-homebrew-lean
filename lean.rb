require "formula"

class Lean < Formula
  homepage "http://leanprover.github.io"
  url "https://github.com/leanprover/lean.git"
  version "0.2.0-git87570740e24a5d96d5d2935bd7c4a02e437f7780"

  bottle do
    root_url 'https://leanprover.github.io/homebrew-lean'
    sha1 '##BOTTLE_YOSEMITE_HASH##' => :yosemite
    sha1 '8d890425a1b15d6aa5c5c436f435c7b481f2cf17' => :mavericks
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
            "-DEMACS_LISP_DIR=/usr/local/share/emacs/site-lisp/lean",
            "-DTCMALLOC=OFF",
            "-DLIBRARY_DIR=./"]
    args << "-DBOOST=ON" if build.with? "boost"
    mkdir 'build' do
      system "cmake", "../src", *args
      system "make", "-j#{ENV.make_jobs}"
      system "make", "-j#{ENV.make_jobs}", "test"
      system "make", "-j#{ENV.make_jobs}", "install"
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
      /usr/local/share/emacs/site-lisp/lean

    To use the Lean Emacs mode, you need to put the following lines in
    your .emacs file:
      (require 'package)
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
      (require 'lean-mode)
    EOS
  end
end
