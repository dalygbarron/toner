(def sample-rate 44100)
(def bit-depth 32)
(def num-channels 2)
(def bytes-per-sample (/ bit-depth 8))

(defn clamp
  "Clamps a value between a minimum and maximum"
  [x low high]
  (max (min x high) low))

(defn lerp
  "Linearly interpolates between two values based on a factor between 0 and 1"
  [start end factor]
  (+ (* (- 1 factor) start) (* factor end)))

(defn sine
  "Musical version of sine function where it cycles every 1 unit"
  [x]
  (math/sin (* x math/pi 2)))

(defn cosine
  "Musical version of cosine function"
  [x]
  (math/cos (* x math/pi 2)))

(defn dist
  "Distorts a signal by increasing it's amplitude and then clipping it"
  [input amp]
  (map (fn [c] (clamp (* c amp) -1 1)) input))

(defn write-float
    "Writes a 32 bit little endian floating point value into a file"
    [file value]
    (file/write file (buffer/push-float32 (buffer) :le value)))

(defn write-int
    "Writes an int with the given number of bytes to a file and it's little end"
    [file value bytes]
    (file/write file (buffer/slice (int/to-bytes (int/s64 value) :le) 0 bytes)))

(defn write-stereo-wav
  "Writes two tuples of floats (left and right channels) to a stereo WAV file"
  [left-samples right-samples filename]
  (def num-samples (length left-samples))
  (assert (= num-samples (length right-samples))
          "Both channels must have the same number of samples")
  (def data-size (* num-samples num-channels bytes-per-sample))
  (def file-size (+ data-size 36))
  (with [f (file/open filename :wb)]
        (file/write f "RIFF")
        (write-int f file-size 4)
        (file/write f "WAVEfmt ")
        (write-int f 16 4)
        (write-int f 3 2)
        (write-int f num-channels 2)
        (write-int f sample-rate 4)
        (write-int f (* sample-rate num-channels bytes-per-sample) 4)
        (write-int f (* num-channels bytes-per-sample) 2)
        (write-int f bit-depth 2)
        (file/write f "data")
        (write-int f data-size 4)
        (for i 0 num-samples
          (each channel [left-samples right-samples]
            (write-float f (get channel i))))
        (file/flush f))
  (print "WAV file written successfully: " filename))

(defn phasor-osc
  "Generates phase and you can change the frequency. never ends."
  []
  (var phase 0)
  (while true
    (def freq (yield phase))
    (+= phase (* freq (/ 1 sample-rate)))
    (%= phase 1)))

(defn square-osc
  "Never ending square wave generating coroutine. expects frequency in."
  []
  (def phasor (fiber/new phasor-osc))
  (var freq 1)
  (while true
    (def phase (resume phasor freq))
    (def value (if (> 0.5 phase) 1 -1))
    (set freq (yield [value value]))))

(defn kick-factory
  "Factory function that returns a fiber which generates a kick drum sound"
  [start-pitch end-pitch extent]
  (fn []
    (def phase-maker (fiber/new phasor-osc))
    (for i 0 (* extent sample-rate)
      (def time (/ i sample-rate))
      (def falloff (- 1 (/ time extent)))
      (def phase (resume phase-maker (lerp end-pitch start-pitch falloff))) 
      (yield (dist [(* (sine phase) falloff)
                    (* (sine phase) falloff)] 5.9)))))

(def repeater (fiber/new (fn []
                           (for i 0 4
                             (def kick (fiber/new (kick-factory (+ 70 (* i 20))
                                                                50
                                                                2)))
                             (while (fiber/can-resume? kick)
                               (def sample (resume kick))
                               (if-not (nil? sample) (yield sample)))))))

(def left-samples @[])
(def right-samples @[])

(def generator (fiber/new (kick-factory 90 100 2)))

(while (fiber/can-resume? repeater)
  (def sample (resume repeater))
  (if (nil? sample) (break))
  (array/push left-samples (get sample 0))
  (array/push right-samples (get sample 1)))

(write-stereo-wav left-samples right-samples "stereo_output.wav")