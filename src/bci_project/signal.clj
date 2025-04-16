(ns bci-project.signal
  (:import [com.github.psambit9791.jdsp.filter Butterworth]))

(defn frequency-range
  [band-name]
  (case band-name
    :delta [0.5 4]
    :theta [4 8]
    :alpha [8 13]
    :beta [13 30]
    :gamma [30 100]
    :all [0.5 100]))

(defn apply-butterworth-filter
  "Apply a Butterworth filter to a signal
   Parameters:
     - signal: Vector of signal values
     - cutoff-low: Low cutoff frequency (Hz)
     - cutoff-high: High cutoff frequency (Hz, use nil for lowpass)
     - srate: Sampling rate (Hz)
     - order: Filter order
     - filter-type: :low-pass, :high-pass, or :band-pass"
  [signal cutoff-low cutoff-high srate order filter-type]
  (let [signal-array (double-array signal)
        butterworth (Butterworth. srate)
        filtered-signal (case filter-type
                          :low-pass (.lowPassFilter butterworth signal-array order cutoff-low)
                          :high-pass (.highPassFilter butterworth signal-array order cutoff-low)
                          :band-pass (.bandPassFilter butterworth signal-array order cutoff-low cutoff-high))]
    (vec filtered-signal)))

(defn filter-by-frequency-band
  [signal band-name srate]
  (let [[low-freq high-freq] (frequency-range band-name)]
    (apply-butterworth-filter signal low-freq high-freq srate 4 :band-pass)))