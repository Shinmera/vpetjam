(org.shirakumo.fraf.trial.release:configure
 :build (:features (:vpetjam-release)
         :prune ("pool/effects/"
                 "pool/workbench/"
                 "pool/trial/"
                 "pool/vpetjam/music/"
                 "pool/vpetjam/sound/"
                 "pool/vpetjam/*/*.ase"
                 "pool/vpetjam/*/*.kra"
                 "pool/music/*.wav"
                 "pool/**/*.*~")
         :copy ("CREDITS.mess" "README.mess"))
 :itch (:user "Shinmera" :project "vegetablemash")
 :system "vpetjam"
 :upload (:targets (:itch)))
