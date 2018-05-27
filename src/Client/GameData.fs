module GameData

module GameData =
    let private words = ["abra"; "new"; "text"; "bla"; "dzen"; "linux"; "google"; "stop";
    "aristotle"; "pattern"; "flight"; "fresh"; "orangutang"; "field"; "favor"; "dinner";
    "cardinal"; "division"; "lizard"; "ancestor"; "dent"; "afternoon"; "vulture"; "grape";
    "severe"; "warm"; "prophesy"; "rastle"; "castle"; "abuse"; "attic"; "neurotic"; "fringe"; 
    "donkey"; "anytime"; "prediction"; "dropping"; "basement"; "jackknife"; "imaginary"; "winter";
    "award"; "equation"; "classic"; "treason"; "awesomeness"; "planet"; "absorbing"; "reason"; "binocular";
    "claw"; "planetary"; "western"; "seven"; "pearl"; "officer"; "purpose"; "clot"; "perplexing"; "desire";
    "absorb"; "playground"; "rapid"; "prophesy"; "sick"; "ark"; "cruel"; "confused"; "operatic";
    "crook"; "allowable"; "bait"; "barbershop"; "treason"; "kitten"; "public"; "love"; "avoid"; "bogeyman";
    "courage"; "ambidextrous"; "grinder"; "firm"; "mary"; "aftermath"; "church"; "lion"; "crispy"; "gift";
    "deception"; "behead"; "gradient"; "humility"; "drifter"; "heater"; "crasher"; "fierce"; "decapitation";
    "americana"; "kidnapper"; "whale"; "knowing"; "ballerina"; "frontier"; "young"; "accomplice"; "honeypot";
    "already"; "accursed"; "weed"; "grinding"; "symbolic"; "crumply"; "reason"; "heavyset"; "vinyl"; "fumbling";
    "hotly"; "wartime"; "fundamental"; "daisy"; "angry"; "shameful"; "barbershop"; "civilization"; "maximum";
    "glutton"; "heartbeat"; "quick"; "gutless"; "drain"; "crypt"; "crack"; "rassling"; "fearsome"; "become";
    "legendary"; "fire"; "open"; "blue"; "hotter"; "curfew"; "intruder"; "antidemocratic"; "discord"; "green";
    "cry"; "esoteric"; "gravitational"; "hamster"; "deer"; "invitation"; "anguish"; "four"; "comatose"; "fearless"; "ideal"; "domino"; "despair"; "honey"; "disrupt"; "hungry"; "intoxicant"; "glamorous"; "attitude"; "sharp"; "trial"; "harvest"; "beyond"; "lottery"; "uptown"; "hairy"; "collarbone"; "blank"; "hug"; "afraid"; "leather"; "pig"; "state"; "great"; "bird"; "goggles"; "martingale"; "timeless"; "detainee"; "house"; "bloodstain"; "damage"; "bump"; "hotly"; "chapterhouse"; "leather"; "future"; "terminus"; "abandon"; "humanly"; "color"; "swindler"; "association"; "square"; "hustle"; "conqueror"; "under"; "functional"; "bull"; "courageous"; "face"; "groundwave"; "mightiest"; "brutal"; "hover"; "monkey"; "lick"; "murderous"; "frequency"; "cut"; "basket"; "apparition"; "choking"; "pitch"; "guidebook"; "serenity"; "champion"; "alternate"; "enemies"; "brutish"; "alcoholic"; "emotion"; "water"; "werewolf"; "crayon"; "hill"; "hide"; "virtual"; "careless"; "shipment"; "bizarre"; "fictional"; "distant"; "cradle"; "wilderness"; "cotton"; "rattle"; "hero"; "square"; "hearing"; "bloodstream"; "decomposition"; "infinite"; "criminal"; "thunder"; "fight"; "rear"; "align"; "bland"; "barn"; "roast"; "event"; "fizz"; "foundation"; "raspberry"; "airspace"; "actuality"; "magic"; "dropping"; "pressure"; 
    "ditch"; "associate"; "blacklist"; "ignorant"; "body"; "god"; "goodbye"; "punch"; "conversion"; "cell"; "flavor"; "charm"; "climax"; "carnal"]

    let getWords () = words

    let firstRoundWords () = words |> List.filter (fun x -> x.Length <= 4)
    let secondRoundWords () = words |> List.filter (fun x -> x.Length > 4 && x.Length <= 6)