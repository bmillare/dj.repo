(ns dj.color.convert
  (:require [primitive-math :as p]))

;; Reference: http://wiki.beyondunreal.com/HSV-RGB_Conversion

(defrecord rgb [^double red ^double green ^double blue])

(defn min3 ^double [^rgb color]
  (p/min (.red color)
         (.green color)
         (.blue color)))

(defn max3 ^double [^rgb color]
  (p/max (.red color)
         (.green color)
         (.blue color)))

(defrecord hsv [^double hue ^double saturation ^double value])

(defn str->long [^String color]
  (read-string (str "0x"
                    (if (= (first color)
                           \#)
                      (subs color 1)
                      color))))

(defn long->str [^long color]
  (let [hex (Integer/toHexString color)]
    (str "#"
         (subs "000000"
               (count hex))
         hex)))

(defn long->rgb [^long color]
  (rgb. (-> color
            (p/bit-shift-right 16)
            (p/bit-and 0x0000ff)
            double
            (p/div 255.0))
        (-> color
            (p/bit-shift-right 8)
            (p/bit-and 0x0000ff)
            double
            (p/div 255.0))
        (-> color
            (p/bit-and 0x0000ff)
            double
            (p/div 255.0))))

(defn rgb->long [^rgb color]
  (p/+ (-> (.red color)
           (p/* 255.0)
           (Math/round)
           long
           (p/bit-shift-left 16))
       (-> (.green color)
           (p/* 255.0)
           (Math/round)
           long
           (p/bit-shift-left 8))
       (-> (.blue color)
           (p/* 255.0)
           long)))

(defn rgb->hsv [^rgb color]
  (let [rgb-min (min3 color)
        rgb-max (max3 color)
        chroma (p/- rgb-max rgb-min)]
    (if (p/zero? chroma)
      (hsv. 0.0 0.0 rgb-max)
      (let [chroma-max (p/div chroma rgb-max)]
        (if (p/== (.red color)
                  rgb-max)
          (let [hue (p/div (p/- (.green color)
                                (.blue color))
                           chroma)]
            (if (p/< hue 0.0)
              (hsv. (p/* 60.0 (p/+ hue 6.0))
                    chroma-max
                    rgb-max)
              (hsv. (p/* 60.0 hue)
                    chroma-max
                    rgb-max)))
          (if (p/== (.green color)
                    rgb-max)
            (hsv. (p/* (p/+ (p/div (p/- (.blue color)
                                        (.red color))
                                   chroma)
                            2.0)
                       60.0)
                  chroma-max
                  rgb-max)
            (hsv. (p/* (p/+ (p/div (p/- (.red color)
                                        (.green color))
                                   chroma)
                            4.0)
                       60.0)
                  chroma-max
                  rgb-max)))))))

(defn hsv->rgb [^hsv color]
  (let [value (.value color)
        chroma (p/* (.saturation color)
                    value)
        hdash (p/div (.hue color)
                     60.0)
        X (p/* chroma (p/- 1.0 (Math/abs (p/- (rem hdash
                                                   2.0)
                                              1.0))))
        rgb-min (p/- value chroma)
        chroma+min (p/+ chroma rgb-min)
        X+min (p/+ X rgb-min)]
    (if (p/< hdash 1.0)
      (rgb. chroma+min
            X+min
            rgb-min)
      (if (p/< hdash 2.0)
        (rgb. X+min
              chroma+min
              rgb-min)
        (if (p/< hdash 3.0)
          (rgb. rgb-min
                chroma+min
                X+min)
          (if (p/< hdash 4.0)
            (rgb. rgb-min
                  X+min
                  chroma+min)
            (if (p/< hdash 5.0)
              (rgb. X+min
                    rgb-min
                    chroma+min)
              (rgb. chroma+min
                    rgb-min
                    X+min))))))))
