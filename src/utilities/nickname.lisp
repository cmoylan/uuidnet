(in-package :cl-user)

(defpackage uuidnet.utilities.nickname
  (:use :cl)
  (:export :make-nickname))

(in-package :uuidnet.utilities.nickname)


(defvar *qualities*
  (list "able" "acid" "angry" "automatic" "awake" "bad" "beautiful" "bent" "bitter"
        "black" "blue" "boiling" "bright" "broken" "brown" "certain" "cheap" "chemical"
        "chief" "clean" "clear" "cold" "common" "complete" "complex" "conscious" "cruel"
        "cut" "dark" "dead" "dear" "deep" "delicate" "dependent" "different" "dirty"
        "dry" "early" "elastic" "electric" "equal" "false" "fat" "feeble" "female"
        "fertile" "first" "fixed" "flat" "foolish" "free" "frequent" "full" "future"
        "general" "good" "gray" "great" "green" "hanging" "happy" "hard" "healthy"
        "high" "hollow" "ill" "important" "kind" "last" "late" "left" "like" "living"
        "long" "loose" "loud" "low" "male" "married" "material" "medical" "military"
        "mixed" "narrow" "natural" "necessary" "new" "normal" "old" "open" "opposite"
        "parallel" "past" "physical" "political" "poor" "possible" "present" "private"
        "probable" "public" "quick" "quiet" "ready" "red" "regular" "responsible"
        "right" "rough" "round" "sad" "safe" "same" "second" "secret" "separate"
        "serious" "sharp" "short" "shut" "simple" "slow" "small" "smooth" "soft" "solid"
        "special" "sticky" "stiff" "straight" "strange" "strong" "sudden" "sweet" "tall"
        "thick" "thin" "tight" "tired" "true" "violent" "waiting" "warm" "wet" "white"
        "wide" "wise" "wrong" "yellow" "young"))

(defvar *propper-nouns*
  (list "angle" "ant" "apple" "arch" "arm" "army" "baby" "bag" "ball" "band"
        "basin" "basket" "bath" "bed" "bee" "bell" "berry" "bird" "blade" "board" "boat"
        "bone" "book" "boot" "bottle" "box" "boy" "brain" "brake" "branch" "brick"
        "bridge" "brush" "bucket" "bulb" "button" "cake" "camera" "card" "cart"
        "carriage" "cat" "chain" "cheese" "chest" "chin" "church" "circle" "clock"
        "cloud" "coat" "collar" "comb" "cord" "cow" "cup" "curtain" "cushion" "dog"
        "door" "drain" "drawer" "dress" "drop" "ear" "egg" "engine" "eye" "face" "farm"
        "feather" "finger" "fish" "flag" "floor" "fly" "foot" "fork" "fowl" "frame"
        "garden" "girl" "glove" "goat" "gun" "hair" "hammer" "hand" "hat" "head" "heart"
        "hook" "horn" "horse" "hospital" "house" "island" "jewel" "kettle" "key" "knee"
        "knife" "knot" "leaf" "leg" "library" "line" "lip" "lock" "map" "match" "monkey"
        "moon" "mouth" "muscle" "nail" "neck" "needle" "nerve" "net" "nose" "nut"
        "office" "orange" "oven" "parcel" "pen" "pencil" "picture" "pig" "pin" "pipe"
        "plane" "plate" "plow" "pocket" "pot" "potato" "prison" "pump" "rail" "rat"
        "receipt" "ring" "rod" "roof" "root" "sail" "school" "scissors" "screw" "seed"
        "sheep" "shelf" "ship" "shirt" "shoe" "skin" "skirt" "snake" "sock" "spade"
        "sponge" "spoon" "spring" "square" "stamp" "star" "station" "stem" "stick"
        "stocking" "stomach" "store" "street" "sun" "table" "tail" "thread" "throat"
        "thumb" "ticket" "toe" "tongue" "tooth" "town" "train" "tray" "tree" "trousers"
        "umbrella" "wall" "watch" "wheel" "whip" "whistle" "window" "wing" "wire" "worm"))


(defun pick-random (collection-name)
  "pick a random element from the given collection"
  (nth (random (length (eval collection-name)))
       (eval collection-name)))


(defun make-nickname ()
  "return a nickname consisting of <quality>-<propper-noun>"
  (format nil "~A-~A-~D"
          (pick-random '*qualities*)
          (pick-random '*propper-nouns*)
          (random 100)))
