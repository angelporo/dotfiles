;;; "Compiled" snippets and support files for `nix-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'nix-mode
                     '(("up"
                        "unpackPhase = ''\n  runHook preUnpack\n  $1\n  runHook postUnpack\n'';\n$0\n"
                        "unpackPhase" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/unpackPhase"
                        nil nil)
                       ("ph"
                        "   phases=\"${1:\\$prePhases unpackPhase patchPhase \\$preConfigurePhases configurePhase \\$preBuildPhases buildPhase checkPhase \\$preInstallPhases installPhase fixupPhase installCheckPhase \\$preDistPhases distPhase \\$postPhases}\";\n"
                        "phases" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/phases"
                        nil nil)
                       ("pp"
                        "patchPhase = ''\n  runHook prePatch\n  $1\n  runHook postPatch\n'';\n$0"
                        "patchPhase" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/patchPhase"
                        nil nil)
                       ("pu"
                        "{ lib\n, stdenv\n, fetchurl\n$1\n$2\n}:\n\nstdenv.mkDerivation rec {\n  pname = \"$3\";\n  version = \"$4\";\n\n  src = fetchurl {\n    url = \"$5\";\n    sha256 = \"${6:\\$\\{lib.fakeSha256\\}}\";\n  };\n\n  nativeBuildInputs = [ ${1:$(replace-regexp-in-string \"  *\" \" \"(subst-char-in-string ?, ? yas-text))} ];\n  buildInputs = [\n    ${2:$(replace-regexp-in-string \"  *\" \" \"(subst-char-in-string ?, ? yas-text))}\n  ];\n\n  meta = with lib; {\n    homepage = \"https://$7\";\n    description = \"$8\";\n    longDescription = ''\n      $9\n    '';\n    license = licenses.${10:$$\n  (yas-choose-value '(\n    \"agpl3\"\n    \"asl20\"\n    \"bsd1\"\n    \"bsd2\"\n    \"bsd3\"\n    \"free\"\n    \"gpl2\"\n    \"gpl2Only\"\n    \"gpl2Plus\"\n    \"gpl3\"\n    \"gpl3Only\"\n    \"gpl3Plus\"\n    \"isc\"\n    \"lgpl21Only\"\n    \"lgpl21Plus\"\n    \"lgpl2Only\"\n    \"lgpl2Plus\"\n    \"lgpl3\"\n    \"lgpl3Only\"\n    \"mit\"\n    \"mpl20\"\n    \"ofl\"\n    \"unfree\"\n  ))};\n    maintainers = with maintainers; [ $11 ];\n    platforms = platforms.${12:$$\n  (yas-choose-value '(\n    \"all\"\n    \"allBut\"\n    \"arm\"\n    \"cygwin\"\n    \"darwin\"\n    \"freebsd\"\n    \"gnu\"\n    \"i686\"\n    \"illumos\"\n    \"linux\"\n    \"mesaPlatforms\"\n    \"mips\"\n    \"netbsd\"\n    \"none\"\n    \"openbsd\"\n    \"unix\"\n    \"x86\"\n  ))};\n  };\n}\n"
                        "package url" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/package_url"
                        nil nil)
                       ("pg"
                        "{ lib\n, stdenv\n, fetchFromGitHub\n, $1\n, $2\n}:\n\nstdenv.mkDerivation rec {\n  pname = \"$3\";\n  version = \"$4\";\n\n  src = fetchFromGitHub {\n    owner = \"$5\";\n    repo = ${6:pname};\n    rev = \"${7:v\\$\\{version\\}}\";\n    sha256 = \"${8:\\$\\{lib.fakeSha256\\}}\";\n  };\n\n  nativeBuildInputs = [ ${1:$(replace-regexp-in-string \"  *\" \" \"(subst-char-in-string ?, ? yas-text))} ];\n  buildInputs = [\n    ${2:$(replace-regexp-in-string \"  *\" \" \"(subst-char-in-string ?, ? yas-text))}\n  ];\n\n  meta = with lib; {\n    homepage = \"https://${9:github.com/$5/$3}\";\n    description = \"$10\";\n    longDescription = ''\n      $11\n    '';\n    license = licenses.${12:$$\n  (yas-choose-value '(\n    \"agpl3\"\n    \"asl20\"\n    \"bsd1\"\n    \"bsd2\"\n    \"bsd3\"\n    \"free\"\n    \"gpl2\"\n    \"gpl2Only\"\n    \"gpl2Plus\"\n    \"gpl3\"\n    \"gpl3Only\"\n    \"gpl3Plus\"\n    \"isc\"\n    \"lgpl21Only\"\n    \"lgpl21Plus\"\n    \"lgpl2Only\"\n    \"lgpl2Plus\"\n    \"lgpl3\"\n    \"lgpl3Only\"\n    \"mit\"\n    \"mpl20\"\n    \"ofl\"\n    \"unfree\"\n  ))};\n    maintainers = with maintainers; [ $12 ];\n    platforms = platforms.${13:$$\n  (yas-choose-value '(\n    \"all\"\n    \"allBut\"\n    \"arm\"\n    \"cygwin\"\n    \"darwin\"\n    \"freebsd\"\n    \"gnu\"\n    \"i686\"\n    \"illumos\"\n    \"linux\"\n    \"mesaPlatforms\"\n    \"mips\"\n    \"netbsd\"\n    \"none\"\n    \"openbsd\"\n    \"unix\"\n    \"x86\"\n  ))};\n  };\n}\n"
                        "package github" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/package_github"
                        nil nil)
                       ("ip"
                        "installPhase= ''\n  runHook preInstall\n${1:    mkdir -p \\$out/bin\n    for f in \\$(find . -executable -type f);\n    do\n      cp \\$f \\$out/bin/\n    done}\n  runHook postInstall\n'';\n$0"
                        "installPhase" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/installPhase"
                        nil nil)
                       ("ic"
                        "installCheckPhase= ''\n  runHook preInstallCheck\n$1\n  runHook postInstallCheck\n'';\n$0"
                        "installCheckPhase" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/installCheckPhase"
                        nil nil)
                       ("fp"
                        "fixupPhase= ''\n  runHook preFixup\n$1\n  runHook postFixup\n'';\n$0"
                        "fixPhase" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/fixupPhase"
                        nil nil)
                       ("dp"
                        "distPhase= ''\n  runHook preDist\n$1\n  runHook postDist\n'';\n$0"
                        "distPhase" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/distPhase"
                        nil nil)
                       ("cp"
                        "configurePhase= ''\n  runHook preConfigure\n$1\n  runHook postConfigure\n'';\n$0"
                        "configurePhase" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/configurePhase"
                        nil nil)
                       ("ch"
                        "checkPhase= ''\n  runHook preCheck\n$1\n  runHook postCheck\n'';\n$0"
                        "checkPhase" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/checkPhase"
                        nil nil)
                       ("ca" "  $1 = callPackage ../$2/$1 { };\n$0"
                        "callPackage" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/callPackage"
                        nil nil)
                       ("bp"
                        "buildPhase= ''\n  runHook preBuild\n$1\n  runHook postBuild\n'';\n$0"
                        "buildPhase" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/nix-mode/buildPhase"
                        nil nil)))


;;; Do not edit! File generated at Tue Sep  9 15:36:34 2025
