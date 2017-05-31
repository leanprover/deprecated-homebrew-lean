class LeanAT02 < Formula
  desc "Interactive, automatable, dependently-typed theorem prover"
  homepage "https://leanprover.github.io"
  url "https://github.com/leanprover/lean2.git",
      :revision => "c73b2860d5211187e9aa1039d1a49dcabdca4292"
  version "0.2.0.20160602210703.gitc73b2860d5211187e9aa1039d1a49dcabdca4292"

  bottle do
    root_url "https://dl.bintray.com/lean/lean"
    sha256 "329689bfef46678547dca02ccc637ed7cd37f6831e0577aa75dc8af0c3231345" => :yosemite
    sha256 "ac9fe7e4f2c1ea33dc6af5dada8c662728f350cad459c69cd833980a92d93fb1" => :el_capitan
  end

  keg_only :versioned_formula

  option "with-boost", "Compile using boost"

  # Required
  depends_on "gmp"
  depends_on "mpfr"
  depends_on "ninja"
  depends_on "cmake" => :build
  depends_on "boost" => [:build, :optional]

  def install
    cmake_args = std_cmake_args + %w[-DTCMALLOC=OFF
                                     -GNinja
                                     -DLIBRARY_DIR=./]
    cmake_args << "-DBOOST=ON" if build.with? "boost"
    mkdir "build" do
      system "curl", "-O", "-L", "https://github.com/leanprover/emacs-dependencies/archive/master.zip"
      system "unzip", "master.zip"
      mv "emacs-dependencies-master", "../src/emacs/dependencies"
      rm "master.zip"
      system "cmake", "../src", *cmake_args
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
                  fill-column-indicator s mmm-mode))
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
    (testpath/"succeed.lean").write <<-EOS.undent
      example (p q : Prop) : p ∧ q → q ∧ p :=
      assume Hpq : p ∧ q,
      have Hp : p, from and.elim_left Hpq,
      have Hq : q, from and.elim_right Hpq,
      show q ∧ p, from and.intro Hq Hp
    EOS

    (testpath/"fail.lean").write <<-EOS.undent
      example (p q : Prop) : p ∧ q → q ∧ p :=
      assume Hpq : p ∧ q,
      show q ∧ p, from and.intro Hpq Hpq
    EOS
    expected_failure_message = <<-EOS.undent
      /fail.lean:3:17: error: type mismatch at application
        and.intro Hpq
      term
        Hpq
      has type
        p ∧ q
      but is expected to have type
        q
    EOS

    # Apparently Lean attempts to lock the library files,
    # so copy the files into the sandbox.
    mkdir_p testpath/".local/lib/lean"
    cp_r prefix/"library", testpath/".local/lib/lean/library"
    ENV["LEAN_PATH"] = testpath/".local/lib/lean/library"

    assert_equal "", shell_output("#{bin}/lean #{testpath}/succeed.lean")

    assert_match expected_failure_message,
                 shell_output("#{bin}/lean #{testpath}/fail.lean",
                              1)
  end
end
