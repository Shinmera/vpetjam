(org.shirakumo.fraf.trial.release:configure
 :build (:features (:vpetjam-release)
         :prune ("pool/effects/"
                 "pool/workbench/"
                 "pool/trial/"
                 "pool/vpetjam/music/"
                 "pool/vpetjam/sound/"
                 "pool/vpetjam/*/*.ase"
                 "pool/music/*.wav"
                 "pool/**/*.*~")
         :copy ("CREDITS.mess" "README.mess"))
 :itch (:user "Shinmera")
 :system "vpetjam")
