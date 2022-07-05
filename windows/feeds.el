(setq elfeed-feeds
      '(("https://recorder.ro/feed/" ro)
        ("http://decatorevista.ro/feed" ro culture)
        ("http://beta.dela0.ro/atom" ro culture)
        ;; ("http://www.edupedu.ro/feed/" ro edu)
        ("http://www.dilemaveche.ro/rss.xml" ro culture)
        ;; ("https://www.digi24.ro/rss" ro)
        ("https://www.europafm.ro/category/stiri/feed/" ro)
        ("http://g4media.ro/feed" ro)
        ("http://utopiabalcanica.net/feed/" ro fun)
        ("http://republica.ro/rss" ro)
		("https://filme-carti.ro/feed" ro culture lit books)
        ;; ("https://life.ro/feed" ro)
        ("http://pressone.ro/feed/" ro)
		;; ("https://www.eff.org/rss/updates.xml" cs privsec)
		;; ("https://freedom-to-tinker.com/feed/rss/" css privsec)
        ("https://thereader.mitpress.mit.edu/feed" cs books blogs)
        ("http://www.newyorker.com/feed/everything" intl)
        ("http://www.newyorker.com/feed/posts" intl)
        ("http://www.newyorker.com/feed/magazine/rss" intl)
        ("http://www.newyorker.com/feed/culture" intl culture)
        ("http://www.newyorker.com/feed/humor" intl fun)
        ("http://www.newyorker.com/feed/books" intl books)
		("https://www.destructoid.com/feed" games)
		("https://www.eurogamer.net/feed" games)
		("https://www.polygon.com/rss/index.xml" games)
		("https://www.escapistmagazine.com/?feed=rss" games)
        ;; ("http://www.newyorker.com/feed/tech" intl tech)
        ;; ("https://www.lemonde.fr/rss/une.xml" intl)
        ;; ("https://www.lemonde.fr/culture/rss_full.xml" intl culture)
        ;; ("https://www.lemonde.fr/international/rss_full.xml" intl)
        ;; ("https://www.lemonde.fr/sciences/rss_full.xml" intl sci)
        ;; ("http://feeds.reuters.com/news/artsculture" intl culture)
        ;; ("http://feeds.reuters.com/reuters/lifestyle" intl)
        ;; ("http://feeds.reuters.com/reuters/oddlyEnoughNews" intl fun)
        ;; ("http://feeds.reuters.com/reuters/scienceNews" intl sci)
        ;; ("http://feeds.reuters.com/reuters/technologyNews" intl tech)
        ;; ("http://feeds.reuters.com/reuters/topNews" intl)
        ;; ("https://blog.veitheller.de/feed.rss" cs blogs)
        ;; ("http://www.overcomingbias.com/feed" ai cs)
        ;; ("https://openlogicproject.org/feed/" math cs)
		("https://3quarksdaily.com/feed" culture sci)
		("https://aldaily.com/feed" culture lit sci)
		("https://feedproxy.google.com/brainpickings/rss" culture lit sci books)
        ("https://www.ziarulmetropolis.ro/feed" culture books ro)
        ("http://andreicraciun.eu/feed" blogs culture)
        ("https://granta.com/feed/" books)
        ("https://lithub.com/feed/" books)
        ("https://arxiv.org/rss/cs.SC/recent" arxiv)
        ("https://arxiv.org/rss/cs.MS/recent" arxiv)
        ("https://arxiv.org/rss/cs.LO/recent" arxiv)
        ("https://arxiv.org/rss/math.LO/recent" arxiv)
        ("https://arxiv.org/rss/math.KT/recent" arxiv)
        ("https://arxiv.org/rss/math.HO/recent" arxiv)
        ("https://arxiv.org/rss/physics.hist-ph/recent" arxiv)
        ("https://www.quantamagazine.org/feed/" sci)
        ("http://nautil.us/rss" sci)
        ;; ("https://haskellweekly.news/haskell-weekly.atom" math cs)
        ("http://feeds.feedburner.com/Math3ma" math)
        ("https://compositionality-journal.org/feed/" math)
        ("https://bor0.wordpress.com/feed/" cs)
        ("http://lambda-the-ultimate.org/rss.xml" cs)
		("https://vincentdelft.be/rss" cs)
        ;; ("https://www.i-bsd.com/feed.xml" cs bsd)
        ("https://opensource.com/feed" cs)
        ("https://www.romanzolotarev.com/n/rss.xml" cs)
        ("http://feeds.cyberciti.biz/Nixcraft-LinuxFreebsdSolarisTipsTricks" cs linux)
        ("https://planet.emacslife.com/atom.xml" emacs)
        ("https://www.with-emacs.com/rss.xml" emacs)
        ("https://nullprogram.com/feed/" emacs)
        ("https://emacsredux.com/atom.xml" emacs)
        ("http://sachachua.com/blog/category/emacs/feed" emacs)
        ("http://rss.slashdot.org/Slashdot/slashdotMain" tech)
        ("http://www.theverge.com/rss/index.xml" tech)
        ("https://news.ycombinator.com/rss" tech)
        ("http://feeds.arstechnica.com/arstechnica/index" tech)
        ("https://hyperliteratura.ro/feed/" books ro)
        ("https://bookhub.ro/feed/" books ro)
        ("https://reasonandmeaning.com/feed/" phil)
        ("https://izvoaredefilosofie.blogspot.com/feeds/posts/default" phil ro)
        ("http://www.brainpickings.org/feed/" phil)
        ("https://www.3ammagazine.com/3am/feed/" phil)
        ;; ("http://feeds.feedburner.com/GentlemansGazette" fun)
        ("http://feeds.feedburner.com/BoredPanda" fun)
        ;; ("http://www.gourmetpens.com/feeds/posts/default" pens)
        ;; ("https://www.gentlemanstationer.com/?format=rss" pens)
        ;; ("https://www.pennonia.eu/feed/" pens)
        ;; ("https://blog.gouletpens.com/feed/" pens)
        ;; ("https://nibbinibnibb.com/feed/" pens)
        ;; ("http://www.avax.news/rss.xml" photo)
        ;; ("https://www.logicmatters.net/feed" blogs cs math phil)
        ;; ("https://paulgabor.com/feed/" blogs ro)
        ;; ("https://patraru.ro/feed/" blogs ro)
        ;; ("https://ioanflorin.wordpress.com/feed/" blogs ro)
        ;; ("https://www.gatesnotes.com/rss/" blogs)
        ("http://ciprianmuntele.ro/feed/" blogs ro)
        ("https://andreicraciun.eu/feed/" blogs ro)
        ("https://viorelilisoi.ro/feed/" blogs ro)
        ;; ("https://eusuntv.ro/feed/" blogs ro)
        ("https://www.catavencii.ro/feed" fun ro)
        ("https://kmkz.ro/rss.xml" fun ro)
        ("https://www.timesnewroman.ro/?act=rss" fun ro)
        ("https://www.scena9.ro/feed" ro culture books)
        ("https://iclita.net/feed/rss/" ro books lit)
        ("http://feeds.feedburner.com/linuxrig" cs linux)
        ("http://www.tuxmachines.org/node/feed" cs linux)
        ("https://lwn.net/headlines/rss" cs linux)
        ("https://increment.com/feed.xml" cs tech)
        ("https://jonathanabennett.github.io/rss.xml" emacs)
        ("https://bitcannon.net/index.xml" cs linux)
        ("https://admiralbumblebee.com/feed.xml" cs guitar)
        ("https://pixls.us/feed.xml" linux foss photo)
        ("https://begriffs.com/atom.xml" linux cs)
        ("https://averagelinuxuser.com/feed.xml" linux cs)
        ("https://bedroomproducersblog.com/feed/" music)
        ;; ("https://www.thomann.de/blog/en/feed/" music)
        ("https://rtalbert.org/rss/" edu math cs)
        ("https://jcs.org/rss" cs linux blogs)
        ("https://www.italianbark.com/interior-design-blog/feed" design)
        ("https://abeautifulmess.com/feed" design)
        ("http://cocolapinedesign.com/feed" design)
        ("https://www.onlydecolove.com/feed" design)
        ("https://stylizimoblog.com/blog/feed" design)
        ("http://feeds.feedburner.com/myscandinavianhome" design)
        ;; ("https://www.mediafax.ro/rss/" ro)
        ;; ("https://www.agerpres.ro/home.rss" ro)
		("https://www.petreanu.ro/feed" blogs ro)
        ("http://tomasp.net/rss.xml" blogs cs phil)
        ("https://h3artbl33d.nl/feed.xml" blogs cs bsd)
        ;; ("https://blog.stephenwolfram.com/feed/" blogs math cs)
        ("https://iam.bettercoffeer.com/rss" blogs food)
        ("https://www.soundguys.com/feed" blogs music)
        ("https://blackmetaldaily.wordpress.com/feed" blogs music)
        ("https://9to5mac.com/feed" cs apple)
        ("https://9to5google.com/feed" cs google)
        ("https://castel.dev/rss.xml" cs math blogs)
        ("https://www.angrymetalguy.com/feed/" music blogs)
        ;; ("https://metal-fi.com/feed" music blogs)
        ("http://www.linusakesson.net/rssfeed.php" cs blogs)
		("https://protesilaos.com/master.xml" emacs cs)
		("https://feeds.feedblitz.com/plperspectives" cs)

		;; ("https://nitter.net/valeriadepaiva/rss" tw cs math)
		;; ("https://nitter.net/tomaspetricek/rss" tw cs math)
		;; ("https://nitter.net/highergeometer/rss" tw math)
		;; ("https://nitter.net/b0rk/rss" tw cs)
		;; ("https://nitter.net/notthebee/rss" tw cs linux)
		;; ("https://nitter.net/bennjordan/rss" tw music)
		;; ("https://nitter.net/jademastermath/rss" tw cs math)
		;; ("https://nitter.net/danghica/rss" tw cs)
		;; ("https://nitter.net/thebaggers/rss" tw cs)
		;; ("https://nitter.net/academicssay/rss" tw math)
		;; ("https://nitter.net/openculture/rss" tw culture)
		;; ("https://nitter.net/sbrebrown/rss" tw pens)
		;; ("https://nitter.net/8ryceclarke/rss" tw cs math)
		;; ("https://nitter.net/ivepetthatdog/rss" tw fun)
		;; ("https://nitter.net/koszuldude/rss" tw math)
		;; ("https://nitter.net/thepracticaldev/rss" tw cs)
		;; ("https://nitter.net/nixcraft/rss" tw linux cs)
		;; ("https://nitter.net/romanzolotarev/rss" tw linux cs)
		;; ("https://nitter.net/pitopos/rss" tw math)
		;; ("https://nitter.net/brainpicker/rss" tw culture)
		;; ("https://nitter.net/qikipedia/rss" tw fun)
		;; ("https://nitter.net/rantonse/rss" tw math)
		;; ("https://nitter.net/five_books/rss" tw books)
		;; ("https://nitter.net/wolframresearch/rss" tw math cs)
		;; ("https://nitter.net/marcelloseri/rss" tw cs)
		;; ("https://nitter.net/climagic/rss" tw cs linux)
		;; ("https://nitter.net/dreugeniacheng/rss" tw math)
		;; ("https://nitter.net/math3ma/rss" tw math)
		;; ("https://nitter.net/dog_rates/rss" tw fun)
		;; ("https://nitter.net/afiscutean/rss" tw cs)
		;; ("https://nitter.net/n_category/rss" tw cs math)
		;; ("https://nitter.net/cvlad/rss" tw cs math)
		;; ("https://nitter.net/bucharestfp/rss" tw cs math)
		;; ("https://nitter.net/auschwitzmuseum/rss" tw culture)
		;; ("https://nitter.net/maxsnew/rss" tw cs math)
		;; ("https://nitter.net/davidad/rss" tw cs math)
		;; ("https://nitter.net/modeloftheory/rss" tw math)
		;; ("https://nitter.net/emilyriehl/rss" tw cs math)
		;; ("https://nitter.net/sigfpe/rss" tw cs math)
		;; ("https://nitter.net/existentialcomics/rss" tw phil fun)
		;; ("https://nitter.net/stevenstrogatz/rss" tw math)
		;; ("https://nitter.net/_julesh_/rss" tw cs math)
		;; ("https://nitter.net/bangolufsen/rss" tw fun music)
		;; ("https://nitter.net/bjmillermd/rss" tw medicine)
		;; ("https://nitter.net/johncarlosbaez/rss" tw cs math)
		;; ("https://nitter.net/philosophymttrs/rss" tw phil)
		;; ("https://nitter.net/theschooloflife/rss" tw culture)
		;; ("https://nitter.net/montblanc_world/rss" tw fun)
		;; ("https://nitter.net/paperblanks/rss" tw fun)
		;; ("https://nitter.net/bartoszmilewski/rss" tw cs math)
		;; ("https://nitter.net/dog_feelings/rss" tw fun)
		;; ("https://nitter.net/fermatslibrary/rss" tw math)
		;; ("https://nitter.net/fjamie013/rss" tw fun)
		;; ("https://nitter.net/jonyiveparody/rss" tw fun)
		
		;; ("https://invidio.us/feed/channel/UCwG9512Wm7jSS6Iqshz4Dpg" yt cs)              ;; ACM Sigplan
        ;; ("https://invidio.us/feed/channel/UCl8UfR1WkSJS666PlNsn6Yg" yt games)           ;; GamerZakh
        ;; ("https://invidio.us/feed/channel/UCEBcDOjv-bhAmLavY71RMHA" yt cs)              ;; Lambda World
		;; ("https://invidio.us/feed/channel/UCQD8Je8RJyu6moCzkaQVzJw" yt music)			;; Zmei3
		;; ("https://invidio.us/feed/channel/UCFk8kgNu_bqsRZewxMGqkzQ" yt emacs)			;; Emacs SF
		;; ("https://invidio.us/feed/channel/UCwuyodzTl_KdEKNuJmeo99A" yt emacs)			;; Emacs Conferences and Hangouts
		;; ("https://invidio.us/feed/channel/UC0uTPqBCFIpZxlz_Lv1tk_g" yt emacs)			;; Protesilaos Stavrou
        ;; ("https://invidio.us/feed/channel/UCbfYPyITQ-7l4upoX8nvctg" yt cs math)         ;; 2 Minute Paper
        ;; ("https://invidio.us/feed/channel/UC2kF6qdHRTM_hDYfEmzkS9w" yt music)           ;; Netherlands Bach Society
        ;; ("https://invidio.us/feed/channel/UC29U_INoIWljh6dIw8m5bCQ" yt fun)		        ;; Utopia Balcanica
        ;; ("https://invidio.us/feed/channel/UCnHEz9DZ6EAof1-DaQGD_Xw" yt fun)             ;; PPPeter
        ;; ("https://invidio.us/feed/channel/UCd6vEDS3SOhWbXZrxbrf_bw" yt fun tech)        ;; SAMTIME
        ;; ("https://invidio.us/feed/channel/UCVHFbqXqoYvEWM1Ddxl0QDg" yt cs android)      ;; Android Developers
        ;; ("https://invidio.us/feed/channel/UClKO7be7O9cUGL94PHnAeOA" yt cs google)       ;; Google Design
        ;; ("https://invidio.us/feed/channel/UC_x5XG1OV2P6uZZ5FSM9Ttw" yt cs google)       ;; Google Developers
        ;; ("https://invidio.us/feed/channel/UCnUYZLuoy1rq1aVMwx4aTzw" yt cs google)       ;; Google Chrome Developers
        ;; ("https://invidio.us/feed/channel/UCPnvXp_A6O7xREKK2cYwhIw" yt linux)           ;; Vagelis Prokopiou
        ;; ("https://invidio.us/feed/channel/UC-V6odR7HzLCuqjYeowPjLA" yt sci)             ;; Nobel Prize
        ;; ("https://invidio.us/feed/channel/UC9DRGV7k0jh5PgRBcM5SVfA" yt food)            ;; RuledMe
        ;; ("https://invidio.us/feed/channel/UClnDI2sdehVm1zm_LmUHsjQ" yt docu)            ;; Biographics
        ;; ("https://invidio.us/feed/channel/UCOKHwx1VCdgnxwbjyb9Iu1g" yt linux)           ;; Blender Guru
        ;; ("https://invidio.us/feed/channel/UClcE-kVhqyiHCcjYwcpfj9w" yt privsec)         ;; LiveOverflow
        ;; ("https://invidio.us/feed/channel/UCpCBa7DpNda1mNKLCb2K8zQ" yt privsec)         ;; Cybering
        ;; ("https://invidio.us/feed/channel/UC3s0BtrBJpwNDaflRSoiieQ" yt privsec)         ;; Hak5
        ;; ("https://invidio.us/feed/channel/UCbgM8ptK_1uvuDhrym8Q13g" yt travel)          ;; Peter Life
        ;; ("https://invidio.us/feed/channel/UCxDZs_ltFFvn0FDHT6kmoXA" yt travel)          ;; Bald & Bankrupt
        ;; ("https://invidio.us/feed/channel/UCnHEz9DZ6EAof1-DaQGD_Xw" yt fun)             ;; PPPeter
        ;; ("https://invidio.us/feed/channel/UCOzXMkNQxMPn175ihckFkHg" yt music)           ;; Frate Gheorghe
        ;; ("https://invidio.us/feed/channel/UCqt8WqcnWLuff9AEDK7T1eg" yt docu ro)         ;; Dela0
        ;; ("https://invidio.us/feed/channel/UCUXTxsU23H02fewbRzHXBgQ" yt music)           ;; CoverSolutions
        ;; ("https://invidio.us/feed/channel/UC5skJN2WYbyYHUmrMQ00LtA" yt music cs)        ;; Mike Hodnik
        ;; ("https://invidio.us/feed/channel/UCW39zufHfsuGgpLviKh297Q" yt docu)            ;; DW Documentary
        ;; ("https://invidio.us/feed/channel/UC5KbWmC93TBhinPLqh5j2kg" yt cs math)         ;; RealPhysics
        ;; ("https://invidio.us/feed/channel/UC1gpkvbirWBKeUFFfAYtQuw" yt music)           ;; Time Shoebridge
        ;; ("https://invidio.us/feed/channel/UCpPe4q50LE8G-fy7-XeQ7ig" yt music)           ;; Alastair Wilson
        ;; ("https://invidio.us/feed/channel/UCYf-CbA0WS_xTiP53uk_Txg" yt music)           ;; xoxinh
        ;; ("https://invidio.us/feed/channel/UCbKM5fcSsaEFZRP-bjH8Y9w" yt music)           ;; vkgoeswild
        ;; ("https://invidio.us/feed/channel/UCnkp4xDOwqqJD7sSM3xdUiQ" yt music)           ;; Adam Neely
        ;; ("https://invidio.us/feed/channel/UCJbQKSflz2w9u4h8zepqTlA" yt music)           ;; RemixSample
        ;; ("https://invidio.us/feed/channel/UCdov0AvcrgWEzV_pAIu52ZA" yt music)           ;; Rishabh Rajan
        ;; ("https://invidio.us/feed/channel/UCXkNod_JcH7PleOjwK_8rYQ" yt music)           ;; Polyphonic
        ;; ("https://invidio.us/feed/channel/UCqBef_x_6Da3Q5JEXBYGz4Q" yt music)           ;; Art of Moog
        ;; ("https://invidio.us/feed/channel/UCshObcm-nLhbu8MY50EZ5Ng" yt music)           ;; Benn and Gear
        ;; ("https://invidio.us/feed/channel/UC3ZR8kjzs1pf6aH_SMiT3Pw" yt music)           ;; Moog Music
        ;; ("https://invidio.us/feed/channel/UCLtD67ljlaeXQMV4sb-YzNA" yt music)           ;; Holistic Songwriting
        ;; ("https://invidio.us/feed/channel/UCRYv1Y9SQaI3dp57syZUdIw" yt music)           ;; once upon a synth
        ;; ("https://invidio.us/feed/channel/UCkf4VIqu3Acnfzuk3kRIFwA" yt cs linux)        ;; gotbletu
        ;; ("https://invidio.us/feed/channel/UCp6NUFV9mSEK6RxUiEVymVg" yt cs linux)        ;; Red Hat Videos
        ;; ("https://invidio.us/feed/channel/UC8cc4pVKVHG7A9fbNsRNrLQ" yt cs linux)        ;; IBM
        ;; ("https://invidio.us/feed/channel/UC1DTYW241WD64ah5BFWn4JA" yt docu)            ;; Sam O'Nella Academy
        ;; ("https://invidio.us/feed/channel/UCNSzfesc7IgWZwg4n6uXr1A" yt fun)             ;; Tucker Budzyn
        ;; ("https://invidio.us/feed/channel/UC9Xdl6CglNwxCZqvwKuE9TA" yt photo)           ;; Shane Milton
        ;; ("https://invidio.us/feed/channel/UCUR1pFG_3XoZn3JNKjulqZg" yt edu)             ;; thoughtbot
        ;; ("https://invidio.us/feed/channel/UC3XTzVzaHQEd30rQbuvCtTQ" yt news fun)        ;; Last Week Tonight
        ;; ("https://invidio.us/feed/channel/UCRwUrd93W7aW1kEC0hjDKjw" yt docu ro)         ;; Să fie lumină
        ;; ("https://invidio.us/feed/channel/UCekQr9znsk2vWxBo3YiLq2w" yt food)            ;; You Suck at Cooking
        ;; ("https://invidio.us/feed/channel/UC88lvyJe7aHZmcvzvubDFRg" yt docu)            ;; Timeline History Documentaries
        ;; ("https://invidio.us/feed/channel/UC-RA5BzE_BnZhf5iVdNF1hA" yt music)           ;; loopop
        ;; ("https://invidio.us/feed/channel/UCl_dlV_7ofr4qeP1drJQ-qg" yt music)           ;; Tantacrul
        ;; ("https://invidio.us/feed/channel/UCh-PyMficPzVAihCJkFJVAA" yt music)           ;; David Bruce Composer
        ;; ("https://invidio.us/feed/channel/UCnkp4xDOwqqJD7sSM3xdUiQ" yt music)           ;; Adam Neely
        ;; ("https://invidio.us/feed/channel/UCLtD67ljlaeXQMV4sb-YzNA" yt music)           ;; Holistic Songwriting
        ;; ("https://invidio.us/feed/channel/UCpKb02FsH4WH4X_2xhIoJ1A" yt music cs)        ;; The Audio Programmer
        ;; ("https://invidio.us/feed/channel/UCdcemy56JtVTrsFIOoqvV8g" yt music)           ;; Andrew Huang
        ;; ("https://invidio.us/feed/channel/UCXuqSBlHAE6Xw-yeJA0Tunw" yt cs)              ;; Linus Tech Tips
        ;; ("https://invidio.us/feed/channel/UC0vBXGSyV14uvJ4hECDOl0Q" yt cs)              ;; Tech Quickie
        ;; ("https://invidio.us/feed/channel/UCSlBEAhmUdUR2dCPG77GFfA" yt guitar music)    ;; axeofcreation
        ;; ("https://invidio.us/feed/channel/UCQ-W1KE9EYfdxhL6S4twUNw" yt music cs)        ;; The Cherno Project
        ;; ("https://invidio.us/feed/channel/UCgTNupxATBfWmfehv21ym-g" yt security cs)     ;; Null Byte
        ;; ("https://invidio.us/feed/channel/UCshObcm-nLhbu8MY50EZ5Ng" yt music)           ;; Benn and Gear
        ;; ("https://invidio.us/feed/channel/UCZiL6BoryLWxyapUuVYW27g" yt linux)           ;; Average Linux User
        ;; ("https://invidio.us/feed/channel/UC8R8FRt1KcPiR-rtAflXmeg" yt music)           ;; Nahre Sol
        ;; ("https://invidio.us/feed/channel/UClOkMtbv2rsgI7p-607jApg" yt guitar)          ;; Plague Scythe Studios
        ;; ("https://invidio.us/feed/channel/UCIRMhnqxvvIf4VVqgkxvVGA" yt ro fun)          ;; Morning Glory
        ;; ("https://invidio.us/feed/channel/UCdZcGRaBV-VRRyU4t6Ur0mw" yt cs phil)         ;; The Ling Space
        ;; ("https://invidio.us/feed/channel/UCi8XrDg1bK_MJ0goOnbpTMQ" yt cs)              ;; budlabs
        ;; ("https://invidio.us/feed/channel/UCMV8p6Lb-bd6UZtTc_QD4zA" yt lisp emacs)      ;; Baggers    
        ;; ("https://invidio.us/feed/channel/UCxkMDXQ5qzYOgXPRnOBrp1w" yt emacs)           ;; Zamansky
        ;; ("https://invidio.us/feed/channel/UCEfFUaIkjbI06PhALdcXNVA" yt emacs)           ;; EmacsCast
        ;; ("https://invidio.us/feed/channel/UC64UiPJwM_e9AqAd7RiD7JA" yt edu)             ;; Today I Found Out
        ;; ("https://invidio.us/feed/channel/UCimiUgDLbi6P17BdaCZpVbg" yt edu)             ;; exurb1a
        ;; ("https://invidio.us/feed/channel/UCYO_jab_esuFRV4b17AJtAw" yt edu)             ;; 3Blue1Brown
        ;; ("https://invidio.us/feed/channel/UCsooa4yRKGN_zEE8iknghZA" yt edu)             ;; TED-Ed
        ;; ("https://invidio.us/feed/channel/UC9-y-6csu5WGm29I7JiwpnA" yt edu cs)          ;; Computerphile
        ;; ("https://invidio.us/feed/channel/UCxQKHvKbmSzGMvUrVtJYnUA" yt cs)              ;; LearnLinux.tv
        ;; ("https://invidio.us/feed/channel/UCWJ2lWNubArHWmf3FIHbfcQ" yt fun)             ;; NBA
        ;; ("https://invidio.us/feed/channel/UC-yVr372UxmlalC9P5Kqp7w" yt fun ro)          ;; ApropoTV
        ;; ("https://invidio.us/feed/channel/UCJ24N4O0bP7LGLBDvye7oCA" yt edu)             ;; Matt D'Avella
        ;; ("https://invidio.us/feed/channel/UC5Gmg-VtFmnP8qLq8V7Pvtg" yt edu)             ;; John Fish
        ;; ("https://invidio.us/feed/channel/UCEgoThiTZG6wbTVA6B1Ksaw" yt fun)             ;; Gentleman's Gazette
        ;; ("https://invidio.us/feed/channel/UCFASHKnkjl7Xa5Nsj9mfvDA" yt culture)         ;; GSG 360
        ;; ("https://invidio.us/feed/channel/UC2eYFnH61tmytImy1mTYvhA" yt cs)              ;; Luke Smith
        ;; ("https://invidio.us/feed/channel/UCVls1GmFKf6WlTraIb_IaJg" yt cs)              ;; DistroTube
        ;; ("https://invidio.us/feed/channel/UCsnGwSIHyoYN0kiINAGUKxg" yt cs)              ;; Wolfgang
        ;; ("https://invidio.us/feed/channel/UCFzGyNKXPAglNq28qWYTDFA" yt cs)              ;; Kai Hendry
        ;; ("https://invidio.us/feed/channel/UCRE3NFNtdjR96-H4QG4U1Fg" yt cs)              ;; Hex DSL
        ;; ("https://invidio.us/feed/channel/UCk9NvmsPBC3lTn_L9kFaylA" yt cs)              ;; iBSD
        ;; ("https://invidio.us/feed/channel/UC3ts8coMP645hZw9JSD3pqQ" yt cs linux)        ;; Andreas Kling (Serenity OS)
        ;; ("https://invidio.us/feed/channel/UCtK5Oe8sHjp6WPcwWuHUVpQ" yt ro fun)          ;; Starea Nației
        ;; ("https://invidio.us/feed/channel/UCKGwDJhaGCd6nP2U5WTh5vg" yt ro)              ;; Europa FM
        ;; ("https://invidio.us/feed/channel/UChDQ6nYN6XyRU-8IEgbym1g" yt ro)              ;; Recorder
        ;; ("https://invidio.us/feed/channel/UCM4rKhv1DqfNRZyWoLAAtIA" yt music)           ;; Adam Ben Ezra
        ;; ("https://invidio.us/feed/channel/UCyabCqc2M3J9HItEroLOTvA" yt fun)             ;; Kmac
        ;; ("https://invidio.us/feed/channel/UC2VzYhDponbU1QhBl_sOYWQ" yt music)           ;; Sean Angus Watson
        ;; ("https://invidio.us/feed/channel/UCYU0rahOsLMO1176sIh7Gfw" yt music)           ;; mike KidLazy
        ;; ("https://invidio.us/feed/channel/UC3I2GFN_F8WudD_2jUZbojA" yt music)           ;; KEXP
        ;; ("https://invidio.us/feed/channel/UCLlMiPx_v5jFahvImDu_EFQ" yt music)           ;; Bassim Karbalaei
        ;; ("https://invidio.us/feed/channel/UCpOYQ1rBMatAVx4YWTvTjOQ" yt pens)            ;; Gourmet Pens
        ;; ("https://invidio.us/feed/channel/UCVbn813ctsoChuTT4LuLqrA" yt pens)            ;; JetPens
        ;; ("https://invidio.us/feed/channel/UCmOb8pxjIoRundRLIABzwIw" yt pens)            ;; Goldspot Pens
        ;; ("https://invidio.us/feed/channel/UCeWDDbfQxKv0Cgq_UNpwYpA" yt pens)            ;; SBRE Brown
        ;; ("https://invidio.us/feed/channel/UCrFPx-OgfWpaFTNdnd-xAAQ" yt pens)            ;; Dr. Fountain Pens
        ;; ("https://invidio.us/feed/channel/UCPdFDFTd6P1a__tAr8CrpCQ" yt pens)))          ;; Goulet Pens

        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCwG9512Wm7jSS6Iqshz4Dpg" yt cs)          ;; ACM Sigplan
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCl8UfR1WkSJS666PlNsn6Yg" yt games)           ;; GamerZakh
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCEBcDOjv-bhAmLavY71RMHA" yt cs)              ;; Lambda World
		("https://www.youtube.com/feeds/videos.xml?channel_id=UCQD8Je8RJyu6moCzkaQVzJw" yt music)			;; Zmei3
		("https://www.youtube.com/feeds/videos.xml?channel_id=UCFk8kgNu_bqsRZewxMGqkzQ" yt emacs)			;; Emacs SF
		("https://www.youtube.com/feeds/videos.xml?channel_id=UCwuyodzTl_KdEKNuJmeo99A" yt emacs)			;; Emacs Conferences and Hangouts
		("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" yt emacs)			;; Protesilaos Stavrou
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbfYPyITQ-7l4upoX8nvctg" yt cs math)         ;; 2 Minute Paper
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2kF6qdHRTM_hDYfEmzkS9w" yt music)           ;; Netherlands Bach Society
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC29U_INoIWljh6dIw8m5bCQ" yt fun)		        ;; Utopia Balcanica
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCnHEz9DZ6EAof1-DaQGD_Xw" yt fun)             ;; PPPeter
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCd6vEDS3SOhWbXZrxbrf_bw" yt fun tech)        ;; SAMTIME
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVHFbqXqoYvEWM1Ddxl0QDg" yt cs android)      ;; Android Developers
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UClKO7be7O9cUGL94PHnAeOA" yt cs google)       ;; Google Design
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC_x5XG1OV2P6uZZ5FSM9Ttw" yt cs google)       ;; Google Developers
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCnUYZLuoy1rq1aVMwx4aTzw" yt cs google)       ;; Google Chrome Developers
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCPnvXp_A6O7xREKK2cYwhIw" yt linux)           ;; Vagelis Prokopiou
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-V6odR7HzLCuqjYeowPjLA" yt sci)             ;; Nobel Prize
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9DRGV7k0jh5PgRBcM5SVfA" yt food)            ;; RuledMe
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UClnDI2sdehVm1zm_LmUHsjQ" yt docu)            ;; Biographics
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCOKHwx1VCdgnxwbjyb9Iu1g" yt linux)           ;; Blender Guru
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UClcE-kVhqyiHCcjYwcpfj9w" yt privsec)         ;; LiveOverflow
        ;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCpCBa7DpNda1mNKLCb2K8zQ" yt privsec)         ;; Cybering
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3s0BtrBJpwNDaflRSoiieQ" yt privsec)         ;; Hak5
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbgM8ptK_1uvuDhrym8Q13g" yt travel)          ;; Peter Life
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxDZs_ltFFvn0FDHT6kmoXA" yt travel)          ;; Bald & Bankrupt
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCnHEz9DZ6EAof1-DaQGD_Xw" yt fun)             ;; PPPeter
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCOzXMkNQxMPn175ihckFkHg" yt music)           ;; Frate Gheorghe
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqt8WqcnWLuff9AEDK7T1eg" yt docu ro)         ;; Dela0
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUXTxsU23H02fewbRzHXBgQ" yt music)           ;; CoverSolutions
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC5skJN2WYbyYHUmrMQ00LtA" yt music cs)        ;; Mike Hodnik
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCW39zufHfsuGgpLviKh297Q" yt docu)            ;; DW Documentary
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC5KbWmC93TBhinPLqh5j2kg" yt cs math)         ;; RealPhysics
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1gpkvbirWBKeUFFfAYtQuw" yt music)           ;; Time Shoebridge
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCpPe4q50LE8G-fy7-XeQ7ig" yt music)           ;; Alastair Wilson
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYf-CbA0WS_xTiP53uk_Txg" yt music)           ;; xoxinh
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbKM5fcSsaEFZRP-bjH8Y9w" yt music)           ;; vkgoeswild
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCnkp4xDOwqqJD7sSM3xdUiQ" yt music)           ;; Adam Neely
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJbQKSflz2w9u4h8zepqTlA" yt music)           ;; RemixSample
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdov0AvcrgWEzV_pAIu52ZA" yt music)           ;; Rishabh Rajan
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXkNod_JcH7PleOjwK_8rYQ" yt music)           ;; Polyphonic
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqBef_x_6Da3Q5JEXBYGz4Q" yt music)           ;; Art of Moog
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCshObcm-nLhbu8MY50EZ5Ng" yt music)           ;; Benn and Gear
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3ZR8kjzs1pf6aH_SMiT3Pw" yt music)           ;; Moog Music
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCLtD67ljlaeXQMV4sb-YzNA" yt music)           ;; Holistic Songwriting
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRYv1Y9SQaI3dp57syZUdIw" yt music)           ;; once upon a synth
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkf4VIqu3Acnfzuk3kRIFwA" yt cs linux)        ;; gotbletu
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCp6NUFV9mSEK6RxUiEVymVg" yt cs linux)        ;; Red Hat Videos
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8cc4pVKVHG7A9fbNsRNrLQ" yt cs linux)        ;; IBM
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1DTYW241WD64ah5BFWn4JA" yt docu)            ;; Sam O'Nella Academy
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCNSzfesc7IgWZwg4n6uXr1A" yt fun)             ;; Tucker Budzyn
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9Xdl6CglNwxCZqvwKuE9TA" yt photo)           ;; Shane Milton
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUR1pFG_3XoZn3JNKjulqZg" yt edu)             ;; thoughtbot
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3XTzVzaHQEd30rQbuvCtTQ" yt news fun)        ;; Last Week Tonight
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRwUrd93W7aW1kEC0hjDKjw" yt docu ro)         ;; Să fie lumină
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCekQr9znsk2vWxBo3YiLq2w" yt food)            ;; You Suck at Cooking
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC88lvyJe7aHZmcvzvubDFRg" yt docu)            ;; Timeline History Documentaries
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-RA5BzE_BnZhf5iVdNF1hA" yt music)           ;; loopop
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCl_dlV_7ofr4qeP1drJQ-qg" yt music)           ;; Tantacrul
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCh-PyMficPzVAihCJkFJVAA" yt music)           ;; David Bruce Composer
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCnkp4xDOwqqJD7sSM3xdUiQ" yt music)           ;; Adam Neely
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCLtD67ljlaeXQMV4sb-YzNA" yt music)           ;; Holistic Songwriting
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCpKb02FsH4WH4X_2xhIoJ1A" yt music cs)        ;; The Audio Programmer
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdcemy56JtVTrsFIOoqvV8g" yt music)           ;; Andrew Huang
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXuqSBlHAE6Xw-yeJA0Tunw" yt cs)              ;; Linus Tech Tips
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0vBXGSyV14uvJ4hECDOl0Q" yt cs)              ;; Tech Quickie
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSlBEAhmUdUR2dCPG77GFfA" yt guitar music)    ;; axeofcreation
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCQ-W1KE9EYfdxhL6S4twUNw" yt music cs)        ;; The Cherno Project
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCgTNupxATBfWmfehv21ym-g" yt security cs)     ;; Null Byte
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCshObcm-nLhbu8MY50EZ5Ng" yt music)           ;; Benn and Gear
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZiL6BoryLWxyapUuVYW27g" yt linux)           ;; Average Linux User
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8R8FRt1KcPiR-rtAflXmeg" yt music)           ;; Nahre Sol
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UClOkMtbv2rsgI7p-607jApg" yt guitar)          ;; Plague Scythe Studios
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCIRMhnqxvvIf4VVqgkxvVGA" yt ro fun)          ;; Morning Glory
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdZcGRaBV-VRRyU4t6Ur0mw" yt cs phil)         ;; The Ling Space
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCi8XrDg1bK_MJ0goOnbpTMQ" yt cs)              ;; budlabs
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMV8p6Lb-bd6UZtTc_QD4zA" yt lisp emacs)      ;; Baggers    
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxkMDXQ5qzYOgXPRnOBrp1w" yt emacs)           ;; Zamansky
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCEfFUaIkjbI06PhALdcXNVA" yt emacs)           ;; EmacsCast
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC64UiPJwM_e9AqAd7RiD7JA" yt edu)             ;; Today I Found Out
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCimiUgDLbi6P17BdaCZpVbg" yt edu)             ;; exurb1a
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" yt edu)             ;; 3Blue1Brown
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsooa4yRKGN_zEE8iknghZA" yt edu)             ;; TED-Ed
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA" yt edu cs)          ;; Computerphile
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxQKHvKbmSzGMvUrVtJYnUA" yt cs)              ;; LearnLinux.tv
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCWJ2lWNubArHWmf3FIHbfcQ" yt fun)             ;; NBA
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-yVr372UxmlalC9P5Kqp7w" yt fun ro)          ;; ApropoTV
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJ24N4O0bP7LGLBDvye7oCA" yt edu)             ;; Matt D'Avella
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC5Gmg-VtFmnP8qLq8V7Pvtg" yt edu)             ;; John Fish
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCEgoThiTZG6wbTVA6B1Ksaw" yt fun)             ;; Gentleman's Gazette
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCFASHKnkjl7Xa5Nsj9mfvDA" yt culture)         ;; GSG 360
        ;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" yt cs)              ;; Luke Smith
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" yt cs)              ;; DistroTube
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsnGwSIHyoYN0kiINAGUKxg" yt cs)              ;; Wolfgang
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCFzGyNKXPAglNq28qWYTDFA" yt cs)              ;; Kai Hendry
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRE3NFNtdjR96-H4QG4U1Fg" yt cs)              ;; Hex DSL
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCk9NvmsPBC3lTn_L9kFaylA" yt cs)              ;; iBSD
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3ts8coMP645hZw9JSD3pqQ" yt cs linux)        ;; Andreas Kling (Serenity OS)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtK5Oe8sHjp6WPcwWuHUVpQ" yt ro fun)          ;; Starea Nației
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKGwDJhaGCd6nP2U5WTh5vg" yt ro)              ;; Europa FM
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UChDQ6nYN6XyRU-8IEgbym1g" yt ro)              ;; Recorder
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCM4rKhv1DqfNRZyWoLAAtIA" yt music)           ;; Adam Ben Ezra
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyabCqc2M3J9HItEroLOTvA" yt fun)             ;; Kmac
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2VzYhDponbU1QhBl_sOYWQ" yt music)           ;; Sean Angus Watson
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYU0rahOsLMO1176sIh7Gfw" yt music)           ;; mike KidLazy
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3I2GFN_F8WudD_2jUZbojA" yt music)           ;; KEXP
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCLlMiPx_v5jFahvImDu_EFQ" yt music)           ;; Bassim Karbalaei
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCpOYQ1rBMatAVx4YWTvTjOQ" yt pens)            ;; Gourmet Pens
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVbn813ctsoChuTT4LuLqrA" yt pens)            ;; JetPens
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCmOb8pxjIoRundRLIABzwIw" yt pens)            ;; Goldspot Pens
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCeWDDbfQxKv0Cgq_UNpwYpA" yt pens)            ;; SBRE Brown
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrFPx-OgfWpaFTNdnd-xAAQ" yt pens)            ;; Dr. Fountain Pens
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCPdFDFTd6P1a__tAr8CrpCQ" yt pens)))          ;; Goulet Pens
