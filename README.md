# DGPatchMaker

A tool to create patches for the LV2 Drumgizmo plugin from existing sample libraries.

## What is the difference to DGEdit?

DGEdit as provided from the developers of DrumGizmo is an editor to create drumkits for DrumGizmo from self-sampled tracks. This means, the drumkit is recorded on multiple tracks, each hit one after the other has rang out. To create a drumkit patch out of these tracks is quite a hassle, therefore the DGEdit helps here with splitting the available tracks on the right locations and assigning them to instruments and so on.

In contrast, DGPatchMaker is designed for creating DrumGizmo patches from existing drum sample libraries in WAV format. So, ready made libraries available open source as well as from commercial companies.

This is not as trivial as it sounds, as every sample library is structured differently and needs to be fit into DrumGizmos model.

## Download 

Version 1.0 (compiled on Ubuntu 18.04 x86_64): [DGPatchMaker 1.0](https://www.onikudaki.net/blog/wp-content/uploads/2020/04/DGPatchMaker.zip)

## Git Clone

When cloning the repository, make sure to checkout the stable versions afterwards. Currently version 1.0 is the latest, so do a 

```
git clone https://github.com/oswald2/DGPatchMaker
git checkout 1.0
```


## Building 

Best option currently is to use the Haskell tool [stack](https://docs.haskellstack.org/). Download and install stack as detailed on the stack homepage.

Get the source code of DGPatchMaker by either cloning the repository or getting the zip of the master branch and unpack it into a directory.

### Dependencies

To build DGPatchMaker, the `sndfile` library is needed. For compiling also the development packages with the header files are needed. E.g. on Ubuntu 18.04, the 
following packages must be installed for compilation:

```
sudo apt install libsndfile1 libsndfile1-dev sndfile-tools
```

### Building DGPatchMaker

Building DGPatchMaker itself is then simply changing into the DGPatchMaker source directory and issue:

```
stack build
```

The first build can take some time as stack downloads packages and compiles them. Subsequent builds will be much faster. The executable can either be called from the build directory via 

```
stack exec DGPatchMaker
```

or the executable can be installed via 

```
stack install
```

which will put DGPatchMaker into `~/.local/bin`. If the executable should be in some other place, just copy it where it should be. 


## DrumGizmo's Model

DrumGizmo works by mapping the recordings of the drum shells via microphones from MIDI notes to these microphone channels (the output channels of DrumGizmo)

### Instruments 

An instrument in DrumGizmo is every distinct playable sound, often mapping to real instruments, but not always. E.g. a kick drum is mapped to one instrument,
while on snares there is often articulation done like normal snare, rimshot, side-stick, snare-roll etc. Each of these are of course in reality played on the same instrument, but are mapped to different DrumGizmo-instruments to provide this articulation.

The instruments contain samples of the hits with different velocities, and often also the same hit with the same velocities so that DrumGizmo can choose the sample to use. Since each real recorded hit sounds a bit different, even if they are the same velocity, this brings more realism to the sounds.

### Drumkit

The drumkit itself then maps all these defined instruments to the output channels. These channels correspond to the microphones with which the drumkit was recorded. On the DrumGizmo kits downloadable from it's homepage, every hit utilises every microphone, just like on a real recording (e.g. a snare hit is also heard on the kick microphone, just quieter). 

On delivered sample libraries this is often not the case, the shells are on their own, but are also present in the overhead- and room-microphones, so these are the only ones with bleed available. This makes the sound "cleaner", more defined, but also a bit less realistic.

### Midimap

The midi-map then specifies, which MIDI note plays which instrument (in the DrumGizmo sense).

# Using DGPatchMaker

**Warning:** *DGPatchMaker's GTK interface still has it's quirks! If you encounter them and know how to fix them, please consider a pull request!*

DGPatchMaker provides two basic workflows:
 
 1. Automatic: this can only be done for drum sample libraries delivered from DrumDrops. This was the initial reason of why DGPatchMaker was developed primarily. DrumDrops libraries have certain conventions in their naming, making it easier to make sense of the structure of the libraries (e.g. in the name of the wav sample is defined which instrument, which articulation, which velocity, and which round-robin round while instruments can be found in distinct folders). As other sample libraries have different conventions, the automatic mode is useless for these libraries and will not work. 
 2. Manual: one can manually create and edit drumkit patches for DrumGizmo. This has been done for the open source Salamander Kit as well as the SM MegaReaper kit, which have very different structure conventions than the DrumDrops libraries. This process is a bit more involved, but is most probably the standard process for all other libraries.

As the automatic workflow is mostly not available, the description is for the manual workflow.

## Automatic Workflow

**NOTE:** *this only works for DrumDrops libraries! And only, if the library itself adheres to the strucuture which is not always the case!*

Currently, three kits from DrumDrops have been converted for DrumGizmo: the Mapex Rock Kit, the Vintage Folk Kit and the Modern Folk Kit. All of them were different in their structure, so different parsers have been developed/modified for them. For new DrumDrops libraries, try all three parsers in the import process. If this doesn't work (errors reported), one may have to do manual interventions. Note that this had to be done for all three kits in some point as there were always some instruments which were not adhereing to their own standard, so it is very likely, that manual intervention is necesssary.

The steps in creating the kits are:

 1. Set the base directory
 2. Set the samples directory
 3. Import DrumDrops kit
 4. Manually refine if necessary
 5. Export the drumkit

### Set the base directory

The DrumDrop kits are coming in different packs. Most of them have a base directory where everything other is stored within, including patches for different sample engines, MIDI loops and the samples themselves. Click "Set Base Directory" and in the file chooser navigate to this directory and select it.

### Set the samples directory 

Depending on the pack of the libraries, the directory containing the samples may be different. Click the "Set Samples Directory" and select the directory containing the samples. For DrumDrops libraries, this directory has often the name "Samples" in it and contains other directories (for each instrument one) which contain the real WAV files. The selection should be for this directory, containing the instrument sub-directories. The automatic import will go over all directories, create an instrument for them and assign hits for them.

### Import DrumDrops Kit

Click the "Import DrumDrops Drumkit" button on top of DGPatchMaker. This will start the import. It is possible, that errors appear, especially when the library does not full adhere to it's naming standards. In that case revert to the Manual Workflow described below.

### Manually refine

Since DrumGizmo underwent a change in it's file format, also the user interface was slightly changed to be able to handle that. The automatic import can't handle the main outputs flag, which are used for the bleed control inside DrumGizmo. These need to be set manually. To understand this setting, one needs to understand how the bleed control works: it reduces the amplitude of the sample played on other channels which are not selected main. So the instrument needs to have set the "Main" tag checked in the channel map for it's own channels, while the other are bleed-controlled. 

E.g. for the Crocell Kit from the DrumGizmo site, the KDrumL instrument is mapped to all channels, but its channels tagged as "Main" are the "KDrumInside" and "KDrumOutside" channels, as these are the native channels the kick goes out to. The output to all other channels will be controlled by the bleed control. 
For cymbals and the hihat, the Crocell Kit defines that their "Main" channels are the "AmbLeft" and "AmbRight" (the room microphones) and all overhead channels ("OHLeft", "OHRight", "OHCenter").

It is also important to add a Drumkit Name and a Description in the relevant fields, before saving the drumkit.

### Exporting the drumkit

Simply select File -> Export Drumkit. The drumkit file will be exported with the specified name to the folder selected in "Base Directory"

## Manual Workflow

The manual workflow consists of the following steps:

 1. Create the instruments
 2. Create the drumkit:
    1. Assign groups
    2. Assign instruments to channels
    3. Assign master flags for the channels for the bleed control 
 3. Create the MIDI-Map
 4. Export the drumkit 
 5. Export the MIDI-Map

In order to show the usage, I created a video which demonstrates the basic usage of DGPatchMaker: [How to use DGPatchMaker to create a patch](https://youtu.be/u1fcU8DzxFs)
