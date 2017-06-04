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
  option "without-test", "Skip build-time tests (Not recommended)"

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
      system "ctest" if build.with? "test"
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
    (testpath/"succeed.hlean").write <<-EOS.undent
      open equiv
      example (A : Type) : ua erfl = eq.idpath A :=
      eq_of_fn_eq_fn !eq_equiv_equiv (is_equiv.right_inv !eq_equiv_equiv erfl)
    EOS

    (testpath/"fail.hlean").write <<-EOS.undent
      open equiv
      example (A : Type) : ua erfl = eq.idpath A :=
      eq_of_fn_eq_fn !eq_equiv_equiv _
    EOS
    expected_synthesis_failure_message = <<-EOS.undent
      /fail.hlean:4:31: error: don't know how to synthesize placeholder
      A : Type
      ⊢ to_fun (eq_equiv_equiv A A) (ua erfl) = to_fun (eq_equiv_equiv A A) (eq.idpath A)
    EOS
    expected_declaration_failure_message = <<-EOS.undent
      /fail.hlean:4:0: error: failed to add declaration 'example' to environment, value has metavariables
      remark: set 'formatter.hide_full_terms' to false to see the complete term
        λ A,
          eq_of_fn_eq_fn … ?M_1
    EOS

    # Apparently Lean attempts to lock the library files,
    # so copy the files into the sandbox.
    mkdir_p testpath/".local/lib/lean"
    cp_r prefix/"library", testpath/".local/lib/lean/hott"
    ENV["HLEAN_PATH"] = testpath/".local/lib/lean/hott"

    assert_equal "", shell_output("#{bin}/lean #{testpath}/succeed.hlean")

    output = shell_output("#{bin}/lean #{testpath}/fail.hlean",
                          1)
    assert_match expected_synthesis_failure_message, output
    assert_match expected_declaration_failure_message, output
  end
end
