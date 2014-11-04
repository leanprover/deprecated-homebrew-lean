require "formula"

class Lean < Formula
  homepage "http://leanprover.github.io"
  head "https://github.com/leanprover/lean.git"

  # Required
  depends_on 'gmp'
  depends_on 'mpfr'
  depends_on 'lua'
  depends_on 'google-perftools' => :optional

  depends_on 'cmake'            => :build
  option     "with-boost", "Compile using installed boost, not the version shipped with mongodb"
  depends_on 'boost'            => [:build, :optional]

  def install
    args = ["-DCMAKE_INSTALL_PREFIX=#{prefix}",
            "-DCMAKE_BUILD_TYPE=Release",
            "-DEMACS_LIB=#{lib}/emacs/site-lisp"]
    args << "-DBOOST=ON" if build.with? "boost"
    mkdir 'build' do
      system "cmake", "../src", *args
      system "make", "-j#{ENV.make_jobs}"
      system "make", "test"
      system "make", "install"
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
      #{opt_lib}/emacs/site-lisp

    To use the Lean Emacs mode, you need to put the following lines in
    your .emacs file:
      (setq auto-mode-alist (cons '("\\\\.v$" . lean-mode) auto-mode-alist))
      (autoload 'lean-mode "lean" "Major mode for editing Lean vernacular." t)
    EOS
  end
end
