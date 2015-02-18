require "formula"

class Lean < Formula
  homepage "http://leanprover.github.io"
  url "https://github.com/leanprover/lean.git"
  version "0.2.0.20150217215544.git74b8499fa92c7995eadb28bfa83972c0d91ba1aa"

  bottle do
    root_url 'https://leanprover.github.io/homebrew-lean'
    sha1 'fa2f16f16524d1ff9fcdfa38b9bc094c8101d458' => :yosemite
    sha1 'dee7b6535facf61139a819c84cc40e0bb553d858' => :mavericks
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
            "-DEMACS_LISP_DIR=#{prefix}/share/emacs/site-lisp/lean",
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
      #{HOMEBREW_PREFIX}/share/emacs/site-lisp/lean

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
      (setq-local lean-emacs-path "#{HOMEBREW_PREFIX}/share/emacs/site-lisp/lean")
      (add-to-list 'load-path (expand-file-name lean-emacs-path))
      (require 'lean-mode)
    EOS
  end
end
