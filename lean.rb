require "formula"

class Lean < Formula
  homepage "http://leanprover.github.io"
  url "https://github.com/leanprover/lean.git"
  version "0.2.0-git05f27b8f0e92e8ea01118bad62ab7c7514d22635"

  bottle do
    root_url 'https://leanprover.github.io/homebrew-lean'
    sha1 'cbefd05d6e9a9439caebe13784897cc5694a591d' => :yosemite
    sha1 '978e895f0082df96f5fd48285f3822277d6375d0' => :mavericks
  end

  # Required
  depends_on 'gmp'
  depends_on 'mpfr'
  depends_on 'lua'
  depends_on 'google-perftools'
  depends_on 'ninja'            => :build
  depends_on 'cmake'            => :build
  option     "with-boost", "Compile using boost"
  depends_on 'boost'            => [:build, :optional]

  def install
    args = ["-DCMAKE_INSTALL_PREFIX=#{prefix}",
            "-DCMAKE_BUILD_TYPE=Release",
            "-DEMACS_LISP_DIR=#{prefix}/share/emacs/site-lisp/lean",
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
      #{prefix}/share/emacs/site-lisp/lean

    To use the Lean Emacs mode, you need to put the following lines in
    your .emacs file:
      (setq auto-mode-alist (cons '("\\\\.v$" . lean-mode) auto-mode-alist))
      (autoload 'lean-mode "lean" "Major mode for editing Lean vernacular." t)
    EOS
  end
end
