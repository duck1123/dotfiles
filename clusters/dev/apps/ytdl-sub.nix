_: {
  services.ytdl-sub = {
    # Shares Pinchflat's NAS folder (already mounted on edgenix as
    # /mnt/pinchflat for Plex); shows land in /mnt/pinchflat/ytdl-sub.
    nfsTarget = "nas";
    nfsSubPath = "Pinchflat";
    nfs.enable = true;

    # Copied from Pinchflat's sources, 2026-10-31.
    subscriptions = {
      "2bd2bth" = "https://www.youtube.com/@2bd2bth/shorts";
      "ADHD Love" = "https://www.youtube.com/@ADHD_love/shorts";
      "al jokes" = "https://www.youtube.com/@aljokes";
      "Alberta Tech" = "https://www.youtube.com/@albertatech";
      "Alexis Dahl" = "https://www.youtube.com/@AlexisDahl";
      "Anthony Vincent" = "https://www.youtube.com/@TenSecondSongs";
      "Arlentric" = "https://www.youtube.com/@Arlentric/shorts";
      "Billiam" = "https://www.youtube.com/@billiam";
      "Brennan Mulligan" = "https://www.youtube.com/@BrennanMulligan/shorts";
      "captainmidnight" = "https://www.youtube.com/@captainmidnight";
      "Casually Comics" = "https://www.youtube.com/@CasuallyComics";
      "Channel Awesome" = "https://www.youtube.com/@ChannelAwesome";
      "Chris Boden" = "https://www.youtube.com/@Physicsduck";
      "Cleo Abram" = "https://www.youtube.com/@CleoAbram";
      "Comic Drake" = "https://www.youtube.com/@ComicDrake";
      "Craft Computing" = "https://www.youtube.com/@CraftComputing";
      "CruisingTheCut" = "https://www.youtube.com/@CruisingTheCut";
      "Dan Povenmire" = "https://www.youtube.com/@Dantible/shorts";
      "Dave Jorgenson" = "https://www.youtube.com/@DaveJorgenson/shorts";
      "Dr. Jordan Breeding" = "https://www.youtube.com/@Drjordanbreeding";
      "Electric Callboy" = "https://www.youtube.com/channel/UCAR30oDTNbJE-Zd2NDg1mTQ";
      "Elle Cordova" = "https://www.youtube.com/@ElleCordova";
      "Emma Thorne" = "https://www.youtube.com/@EmmaThorneVideos";
      "Etymology Nerd" = "https://www.youtube.com/@etymology_nerd";
      "Farming While Beige" = "https://www.youtube.com/@blackbirdcoop";
      "gabi belle" = "https://www.youtube.com/@itsgabibelle";
      "Internet Today" = "https://www.youtube.com/@InternetTodayTV";
      "It's Just Bits" = "https://www.youtube.com/@ItsJustBits1/shorts";
      "John Harvie" = "https://www.youtube.com/channel/UCNZuNzjpqHjgdhkyeNrAtTw";
      "Josh Johnson" = "https://www.youtube.com/@JoshJohnsonComedy";
      "just joe is fine" = "https://www.youtube.com/@just_joe_is_fine/shorts";
      "LastWeekTonight" = "https://www.youtube.com/@LastWeekTonight";
      "LegalEagle" = "https://www.youtube.com/@LegalEagle";
      "Linus Tech Tips" = "https://www.youtube.com/@LinusTechTips";
      "Luke Stephens" = "https://www.youtube.com/@LukeStephensTV/shorts";
      "Master Builder Alec" = "https://www.youtube.com/@masterbuilderalec";
      "Mike Rafi - Personal Injury Lawyer" = "https://www.youtube.com/@MikeRafiLawyer/shorts";
      "Miniminuteman" = "https://www.youtube.com/@miniminuteman773";
      "Mrwhosetheboss" = "https://www.youtube.com/@Mrwhosetheboss";
      "Nando v Movies" = "https://www.youtube.com/@NandovMovies";
      "NerdSync" = "https://www.youtube.com/NerdSyncProductions";
      "NetworkChuck" = "https://www.youtube.com/@NetworkChuck";
      "Notgabbyb" = "https://www.youtube.com/@notgabbyb";
      "Refashioned Hippie" = "https://www.youtube.com/@refashionedhippie/shorts";
      "Rudy Ayoub" = "https://www.youtube.com/@RudyAyoub";
      "Ryan George Extra Plus!" = "https://www.youtube.com/@ryangeorgeplus";
      "SciManDan" = "https://www.youtube.com/@SciManDan";
      "Scott Cramer" = "https://www.youtube.com/@ScottCramer";
      "Scott The Woz" = "https://www.youtube.com/@ScottTheWoz";
      "Sidney Morss" = "https://www.youtube.com/@sidneymorss/shorts";
      "sullvn" = "https://www.youtube.com/channel/UCdR4Rv5gxp1Xei8uzzP1u2A";
      "TankTheTech" = "https://www.youtube.com/@TankTheTech";
      "Technology Connections" = "https://www.youtube.com/@TechnologyConnections";
      "The Boy Detective" = "https://www.youtube.com/@theboydetective8747";
      "The Charismatic Voice" = "https://www.youtube.com/@TheCharismaticVoice";
      "The Dadvocate" = "https://www.youtube.com/@thedadvocate/shorts";
      "The Daily Show" = "https://www.youtube.com/@TheDailyShow";
      "The Panda Redd" = "https://www.youtube.com/@thepandaredd";
      "The Ryan George Show" = "https://www.youtube.com/@RyanGeorgeShow";
      "The Speech Prof" = "https://www.youtube.com/@TheRealSpeechProf";
      "Treehouse Detective" = "https://www.youtube.com/@treehousedetective/shorts";
      "Viva La Dirt League" = "https://www.youtube.com/@VivaLaDirtLeague";
      "Void Darc" = "https://www.youtube.com/@VoidDarc/shorts";
      "xkcd's What If" = "https://www.youtube.com/@xkcd_whatif";
      "Zack Shutt" = "https://www.youtube.com/@ZackShutt/shorts";
    };
  };
}
