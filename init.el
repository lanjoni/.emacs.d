;; package --- Summary
;;; Commentary:

;; Emacs configuration
;;
;; file: ~/.emacs.d/init.el
;; author: João Augusto Lissoni Lanjoni <joaoaugustolanjoni@gmail.com>
;; date: Oct 29, 2023

;;; Code:

(load-file "~/.emacs.d/packages.el")
(require 'lanjoni-packages)

;; Packages output
(setq packout-file "~/.emacs.d/packout.el")
(load-file packout-file)

(load-file "~/.emacs.d/workflow.el")
(require 'lanjoni-workflow)

(load-file "~/.emacs.d/visual.el")
(require 'lanjoni-visual)

(load-file "~/.emacs.d/elcord.el")

(splash-screen)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "ba111a1e27970b905958219697ba50a78381c4f51d05eaf6fa6c0bdd5700847b" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "72cae321a404adaf88f4a7df9bfbd8e578f6ef6663c3ac66f628e612b292588a" "c03d60937e814932cd707a487676875457e0b564a615c1edfd453f23b06fe879" "9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" "c97a653cd5172dc3606b461d32940460ddf1790260abfe080dcab561ff66fafb" "47d5324dac28a85c1bb84b4c1dc3a8dc407cc7369db6e30d3244b16232b1eec4" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "c92a0fece9ad256d83a0ce85df1f15e1c9280eba91c0cc06f8879b9572a855c7" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "ddffe74bc4bf2c332c2c3f67f1b8141ee1de8fd6b7be103ade50abb97fe70f0c" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "2853dd90f0d49439ebd582a8cbb82b9b3c2a02593483341b257f88add195ad76" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414" "2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" "f458b92de1f6cf0bdda6bce23433877e94816c3364b821eb4ea9852112f5d7dc" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "ce4234c32262924c1d2f43e6b61312634938777071f1129c7cde3ebd4a3028da" "bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" default))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(nezburn-theme ligature crystal-mode nordic-night-theme all-the-icons-ivy-rich org-bullets soothe-theme twilight-theme ubuntu-theme ameba magit treemacs-magit treemacs-nerd-icons treemacs-all-the-icons nerd-icons evil scala-mode php-mode elixir-mode clojure-mode treemacs doom-themes gruber-darker-theme rust-mode emmet-mode add-node-modules-path prettier-js web-mode react-snippets csharp-mode dockerfile-mode go-mode typescript-mode lsp-tailwindcss lsp-treemacs lsp-ui lsp-mode elcord almost-mono-themes svelte-mode projectile gdscript-mode all-the-icons flycheck company dashboard auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
