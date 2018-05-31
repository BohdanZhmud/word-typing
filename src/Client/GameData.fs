module GameData
let private words = ["abra"; "new"; "gum"; "dot"; "hot"; "text"; "bla"; "dzen"; "linux"; "google"; "stop";
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
"ditch"; "associate"; "blacklist"; "ignorant"; "body"; "god"; "goodbye"; "punch"; "conversion"; "cell"; "flavor"; "charm"; "climax"; "carnal"; "arrows"; "grieving"; "encrypt"; "billionaire"; "futuristic"; "hood"; "bacteria"; "billion"; "confusion"; "wreckage"; "mutant"; "child"; "celebrity"; "cruel"; "fermentation"; "observer"; "galloping"; "tornado"; "courageous"; "smart"; "cruel"; "propeller"; "twelve"; "wife"; "giant"; "horseradish"; "robotic"; "barn"; "desolate"; "amputation"; "mountain"; "human"; "groan"; "fathead"; "reckless"; "national"; "jackknife"; "junk"; "absorb"; "villain"; "disclosure"; "drug"; "choker"; "crisp"; "elbow"; "deformity"; "collar"; "skyline"; "smut"; "bleakly"; "sideways"; "clean"; "disfigured"; "harplike"; "junior"; "gang"; "derelict"; "grim"; "guidebook"; "web"; "blinding"; "general"; "vibrator"; "honeypot"; "bizarre"; "hotel"; "conqueror"; "crumply"; "gamble"; "amnesiac"; "zoo"; "addiction"; "gravitational"; "forest"; "gold"; "gimmick"; "promised"; "audio"; "prong"; "effective"; "auxiliary"; "cricket"; "magnetic"; "drama"; "wig"; "fortress"; "near"; "conclusion"; "fold"; "pagan"; "shake"; "murderous"; "global"; "hopper"; "comrade"; "container"; "confused"; "ambitious"; "grab"; "hairless"; "prophetic"; "any"; "fornicator"; "grave"; "goodbye"; "compassionate"; "afraid"; "gun"; "behold"; "disgusting"; "chisel"; "below"; "bucket"; "bead"; "gripping"; "awkward"; "bladder"; "bitter"; "city"; "beard"; "flake"; "harsh"; "basket"; "houseguest"; "necrotic"; "calculation"; "overt"; "director"; "detachable"; "fluent"; "coastal"; "young"; "final"; "charm"; "expansion"; "fish"; "firstborn"; "facade"; "authentic"; "martingale"; "chain"; "ambulatory"; "chop"; "pork"; "damn"; "master"; "high"; "sexual"; "pharaoh"; "critical"; "domino"; "haunting"; "guidebook"; "simple"; "broken"; "moan"; "rear"; "column"; "national"; "projection"; "bait"; "contagious"; "harm"; "airship"; "bully"; "moment"; "ultimate"; "delicious"; "zebra"; "hatchet"; "agonizing"; "heavenly"; "circus"; "villain"; "spell"; "how"; "bet"; "bed"; "tin"; "eel"; "wax"; "bat"; "ivy"]

let getWords round =
    let w = 
        match round with
        | 1 -> words |> List.filter (fun x -> x.Length <= 3)
        | 2 -> words |> List.filter (fun x -> x.Length = 4)
        | 3 -> words |> List.filter (fun x -> x.Length = 5)
        | _ -> words |> List.filter (fun x -> x.Length > 5)
    w |> Utils.shuffleList |> List.take 20