# Brain data in Clojure
This clay notebook is analysis and visualization of an existing brain study from 2017 that explores left and right hand motor imagery (MI) data. The goal is to process, analyze, and visualize EEG signals in order to better understand the brain's electrical activity during MI tasks, and compare that activity to activity during actual movement events. By leveraging various Clojure tools and libraries, this project aims to make it easy for researchers to explore and extract meaningful insights from these brain data sets.

__Prerequisites__ 
    To run this project, you'll need:

    Clojure: 
    The primary language for this project. You can install Clojure by following the official installation guide here: https://clojure.org/guides/install_clojure

    Babashka: 
    A scripting tool for clojure used to facilitate faster execution of Clojure scripts, especially for tasks that involve running Clojure code outside of the JVM. You can install Babashka here https://babashka.org/. Alternatively you can visit the study links and download each matlab file individually; they are each approximately 2-300 mb in size, so be aware of that before running the babashka script and ensure you have the proper amount of hard drive space to allot for the data.

## How to Use
1. Download the MATLAB Files
The first step is to download the necessary MATLAB .mat files containing the EEG data. A Babashka script is included in this project to automate the downloading process. Run the script to retrieve the dataset from the study.

2. Load EEG Data
Once the dataset has been downloaded, the next step is to load the EEG data from the MATLAB .mat file into your Clojure environment. You can do this by calling the load-eeg-data! function. Here’s an example:

   `(load-eeg-data! "path/to/your/file.mat")`

    This will load the data into the eeg-data-atom and preprocess it for further analysis.

3. Visualization
The following function will read whatever mat file was loaded into the eeg-data-atom and display the movement and imagery data for selected sensor locations at a specified time range in the comprehensive-eeg-analysis visualization function.

   `(comprehensive-eeg-analysis brain/eeg-data-atom)`

## To Do
The next things that I would like to do with this data set are to:

- Clearly display specific event and imagery timings at interesting moments.

- Create reactive components that are capable of letting users view specific channels or time ranges of interest without changing the underlying code.

- Display a heatmap of the active parts of the brain according to the sensors.

- Reinforce the mathematical findings with an automated analysis of the data displayed above the relevant charts.

#### Images and data taken from:
Cho, H., Ahn, M., Ahn, S., Moonyoung Kwon, & Jun, S. C. (2017). Supporting data for "EEG datasets for motor imagery brain computer interface" [Data set]. GigaScience Database. [^1]

[^1]: [https://doi.org/10.5524/100295](https://doi.org/10.5524/100295)