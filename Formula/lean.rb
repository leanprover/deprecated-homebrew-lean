class Lean < Formula
  desc "Interactive, automatable, dependently-typed theorem prover"
  homepage "https://leanprover.github.io"
  url "https://github.com/leanprover/lean/archive/v3.2.0.tar.gz"
  sha256 "2668e075a9bdf4270d75fd60181078b33c078c7288b5bb8ad795819d8d5272cf"

  bottle do
    cellar :any
    root_url "https://dl.bintray.com/lean/lean"
    sha256 "02913168261e31ffc4fad059ae6d0f45488de898c63fc4eebbab7c937c04c8e4" => :sierra
  end

  option "with-test", "Run test suite at build-time" # TODO: check each new release to see if we can change the default from skipping the tests to running them.

  # Required
  depends_on "gmp"
  depends_on "ninja" => :build
  depends_on "cmake" => :build

  def install
    cmake_args = std_cmake_args + %w[-DTCMALLOC=OFF
                                     -GNinja
                                     -DLIBRARY_DIR=./]
    mkdir "build" do
      system "cmake", "../src", *cmake_args
      system "ninja", "clean"
      system "ninja"
      system "ctest" if build.with? "test"
      system "ninja", "install"
    end
  end

  def caveats; <<-EOS.undent
    To use the Lean Emacs mode, you need to put the following lines in
    your .emacs file:

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

    assert_equal "", shell_output("#{bin}/lean #{testpath}/succeed.lean")

    assert_match expected_failure_message,
                 shell_output("#{bin}/lean #{testpath}/fail.lean",
                              1)
  end
end
