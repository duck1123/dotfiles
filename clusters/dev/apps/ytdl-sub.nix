_:
let
  videos = url: "${url}/videos";
  shorts = url: "${url}/shorts";
  # A list, because a bare channel URL never gets past the Videos tab. A tab the
  # channel lacks only logs an error.
  both = url: [
    (videos url)
    (shorts url)
  ];
in
{
  services.ytdl-sub = {
    # The NAS's Pinchflat share (mounted on edgenix as /mnt/pinchflat for
    # Plex); shows land in /mnt/pinchflat/ytdl-sub.
    nfsTarget = "nas";
    nfsSubPath = "Pinchflat";
    nfs.enable = true;

    # Copied from Pinchflat's sources, 2026-10-31.
    subscriptions = {
      "2bd2bth" = shorts "https://www.youtube.com/@2bd2bth";
      "ADHD Love" = shorts "https://www.youtube.com/@ADHD_love";
      "al jokes" = shorts "https://www.youtube.com/@aljokes";
      "Alberta Tech" = shorts "https://www.youtube.com/@albertatech";
      "Alexis Dahl" = both "https://www.youtube.com/@AlexisDahl";
      "Anthony Vincent" = both "https://www.youtube.com/@TenSecondSongs";
      "Arlentric" = both "https://www.youtube.com/@Arlentric";
      "Big Tugg" = both "https://www.youtube.com/@biggtugg";
      "Billiam" = videos "https://www.youtube.com/@billiam";
      "Bilmuri" = videos "https://www.youtube.com/@Bilmuriband";
      "Blumineck" = shorts "https://www.youtube.com/@blumineck";
      "Brennan Mulligan" = shorts "https://www.youtube.com/@BrennanMulligan";
      "Brodie Robertson" = both "https://www.youtube.com/@brodierobertson";
      "captainmidnight" = videos "https://www.youtube.com/@captainmidnight";
      "Casually Comics" = videos "https://www.youtube.com/@CasuallyComics";
      "catieosaurus" = both "https://www.youtube.com/@catieosaurus";
      "Channel Awesome" = videos "https://www.youtube.com/@ChannelAwesome";
      "Chris Boden" = shorts "https://www.youtube.com/@Physicsduck";
      "Cleo Abram" = both "https://www.youtube.com/@CleoAbram";
      "Coffee Stain Studios" = both "https://www.youtube.com/@CoffeeStainStudios";
      "Comic Drake" = both "https://www.youtube.com/@ComicDrake";
      "Comic Tropes" = both "https://www.youtube.com/@ComicTropes";
      "Craft Computing" = both "https://www.youtube.com/@CraftComputing";
      "CruisingTheCut" = both "https://www.youtube.com/@CruisingTheCut";
      "Dan Povenmire" = both "https://www.youtube.com/@Dantible";
      "Dave Jorgenson" = both "https://www.youtube.com/@DaveJorgenson";
      "Dominic Noble" = both "https://www.youtube.com/@Dominic-Noble";
      "Dr. Esme Louise James" = both "https://www.youtube.com/@dr.esme.louise";
      "Dr. Jordan Breeding" = both "https://www.youtube.com/@Drjordanbreeding";
      "Drumeo" = videos "https://www.youtube.com/@DrumeoOfficial";
      "Dubioza Kolektiv" = both "https://www.youtube.com/@dubiozatv";
      "El Estepario Siberiano" = both "https://www.youtube.com/channel/UCskD50ri3723Tk1SkS-qAIA";
      "Electric Callboy" = both "https://www.youtube.com/channel/UCAR30oDTNbJE-Zd2NDg1mTQ";
      "Elle Cordova" = both "https://www.youtube.com/@ElleCordova";
      "Emma Thorne" = both "https://www.youtube.com/@EmmaThorneVideos";
      "Etymology Nerd" = both "https://www.youtube.com/@etymology_nerd";
      "Fact Fiend" = both "https://www.youtube.com/@FactFiend";
      "Farming While Beige" = both "https://www.youtube.com/@blackbirdcoop";
      "gabi belle" = both "https://www.youtube.com/@itsgabibelle";
      "GANG!" = both "https://www.youtube.com/@gangallcaps";
      "Hank Green" = both "https://www.youtube.com/@hankschannel";
      "hbomberguy" = both "https://www.youtube.com/@hbomberguy";
      "History Unhinged" = videos "https://www.youtube.com/@HistoryUnhingedPod";
      "Internet Today" = both "https://www.youtube.com/@InternetTodayTV";
      "It's Just Bits" = both "https://www.youtube.com/@ItsJustBits1";
      "J. Draper" = both "https://www.youtube.com/@JDraper";
      "Jenny Nicholson" = both "https://www.youtube.com/@JennyNicholson";
      "Joe Scott" = both "https://www.youtube.com/@joescott";
      "John Harvie" = both "https://www.youtube.com/channel/UCNZuNzjpqHjgdhkyeNrAtTw";
      "Jordan Howlett" = both "https://www.youtube.com/@jordan_the_stallion8";
      "Josh Johnson" = both "https://www.youtube.com/@JoshJohnsonComedy";
      "just joe is fine" = both "https://www.youtube.com/@just_joe_is_fine";
      "Ladrones" = both "https://www.youtube.com/@ladronesmx";
      "LastWeekTonight" = both "https://www.youtube.com/@LastWeekTonight";
      "LegalEagle" = both "https://www.youtube.com/@LegalEagle";
      "Lindsay Ellis" = both "https://www.youtube.com/@LindsayEllisVids";
      "Lindsay Nikole" = both "https://www.youtube.com/@LindsayNikole";
      "Linus Tech Tips" = both "https://www.youtube.com/@LinusTechTips";
      "Low End University" = both "https://www.youtube.com/@LowEndUniversity";
      "Luke Stephens" = both "https://www.youtube.com/@LukeStephensTV";
      "Master Builder Alec" = both "https://www.youtube.com/@masterbuilderalec";
      "Melodicka Bros" = both "https://www.youtube.com/@melodickabros";
      "Mike Rafi - Personal Injury Lawyer" = both "https://www.youtube.com/@MikeRafiLawyer";
      "Miniminuteman" = both "https://www.youtube.com/@miniminuteman773";
      "Mrwhosetheboss" = both "https://www.youtube.com/@Mrwhosetheboss";
      "Nando v Movies" = both "https://www.youtube.com/@NandovMovies";
      "NerdSync" = both "https://www.youtube.com/NerdSyncProductions";
      "NetworkChuck" = both "https://www.youtube.com/@NetworkChuck";
      "Ninja Sex Party" = both "https://www.youtube.com/@nsp";
      "NixCon" = both "https://www.youtube.com/@NixCon";
      "Notgabbyb" = both "https://www.youtube.com/@notgabbyb";
      "Polyphia" = both "https://www.youtube.com/@Polyphia";
      "Refashioned Hippie" = both "https://www.youtube.com/@refashionedhippie";
      "Rerez" = both "https://www.youtube.com/user/rereztv";
      "Rudy Ayoub" = both "https://www.youtube.com/@RudyAyoub";
      "Ryan George" = both "https://www.youtube.com/@RyanGeorge";
      "Ryan George Extra Plus!" = both "https://www.youtube.com/@ryangeorgeplus";
      "SciManDan" = both "https://www.youtube.com/@SciManDan";
      "Scott Cramer" = both "https://www.youtube.com/@ScottCramer";
      "Scott The Woz" = both "https://www.youtube.com/@ScottTheWoz";
      "Sidney Morss" = both "https://www.youtube.com/@sidneymorss";
      "serviceNow Dev Program" = both "https://www.youtube.com/@ServiceNowDevProgram";
      "Slope's Game Room" = videos "https://www.youtube.com/@SlopesGameRoom";
      "Stand-up Maths" = both "https://www.youtube.com/@standupmaths";
      "sullvn" = videos "https://www.youtube.com/channel/UCdR4Rv5gxp1Xei8uzzP1u2A";
      "TankTheTech" = both "https://www.youtube.com/@TankTheTech";
      "Technology Connections" = both "https://www.youtube.com/@TechnologyConnections";
      "The Boy Detective" = both "https://www.youtube.com/@theboydetective8747";
      "The Charismatic Voice" = both "https://www.youtube.com/@TheCharismaticVoice";
      "The Dadvocate" = both "https://www.youtube.com/@thedadvocate";
      "The Daily Show" = both "https://www.youtube.com/@TheDailyShow";
      "The Modern Rogue" = both "https://www.youtube.com/@ModernRogue";
      "The Panda Redd" = both "https://www.youtube.com/@thepandaredd";
      "The Ryan George Show" = both "https://www.youtube.com/@RyanGeorgeShow";
      "The Speech Prof" = both "https://www.youtube.com/@TheRealSpeechProf";
      "TomBacons" = shorts "https://www.youtube.com/@TomBacons";
      "Tom Cardy" = both "https://www.youtube.com/@tomcardy1";
      "Treehouse Detective" = both "https://www.youtube.com/@treehousedetective";
      "UFD Tech" = shorts "https://www.youtube.com/@UFDTech";
      "Unprocessed" = both "https://www.youtube.com/@Unprocessed";
      "Viva La Dirt League" = both "https://www.youtube.com/@VivaLaDirtLeague";
      "Void Darc" = both "https://www.youtube.com/@VoidDarc";
      "xkcd's What If" = both "https://www.youtube.com/@xkcd_whatif";
      "Zack Shutt" = both "https://www.youtube.com/@ZackShutt";
    };
  };
}
