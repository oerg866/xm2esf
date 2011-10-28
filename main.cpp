#include <iostream>
#include <fstream>
#include <string>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <stdint.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdlib.h>

/**
    XM 2 ESF   by Oerg866   C++ Port
                                    **/


using namespace std;

#define pi = 3.14159265358979323846264338;
#define version = "0.99a C++ Port WIP";

string trim(string p, const char* t = " tnrfv")
{
    string s;
    s = p;
    s.erase(0, s.find_first_not_of(t));
    s.erase(s.find_last_not_of(t) + 1);
    return s;
}

//string lcase(string ppp) {
//    for (uint32_t i = 0; i < ppp.size; i++) {
//        ppp.substr(i,1) = tolower(ppp.substr(i,1));
//    }
//    return (ppp);
//}

string spleft(string strn)
{
    uint16_t i = 0;
    string car;
    string comp;
    string speft;
    car = strn[0];
    if (car[0] == 0x34)
    {
        comp = "\"";
        i = 1;
        car = strn[1];
    }
    else
    {
        comp = " ";
    }

    while (car != comp)
    {
        speft = speft + car;
        i++;
        car = strn.substr(i, 1);
        if ((i+1) > strn.size))
        {
            return (speft + car);
        }
    }
    return (speft);
}

string param(string strn, uint16_t b)
{
    uint16_t c = 0;
    uint16_t i = 0;
    string comp;
    string speft;
    string car;
    car = strn[0];
    if (car[0] = 0x34)
    {
        comp = "\"";
        i = 1;
        car = strn[1];
    }
    else
    {
        comp = " ";
    }

    while (c != b)
    {
        while (car != comp)
        {
            speft = speft + car;
            i++;
            car = strn[i];
            if ((i+1) > sizeof(strn))
            {
                speft = "";
                return (speft);
            }
        }
        i++;
        c++;
        car = speft;
    }
    return trim(spleft(speft.substr(i)));
}

int main(int argc, char *argv[])
{

    cout << "XM2ESF - Convert XM to Echo Stream Format\n";
    cout << "*** BETA VERSION " << version << "\n";
    cout << "\n";
    cout << "Copyright (C) 2011 Oerg866\n";
    cout << "\n";
    cout << "Echo (C) 2011 Sik      http://echo.mdscene.net (Website N/A yet)\n";
    cout << "\n";
    cout << "PLEASE REPORT BUGS TO GITHUB OR oerg866@tototek.com!!\n";
    cout << "\n";
    cout << "This program is freeware. It must not be sold. This is a temporary\n";
    cout << "disclaimer until we actually bother setting up license stuffs =P\n";
    cout << "\n";
    cout << "Please take a few seconds to acknowledge that this\n";
    cout << "Software is currently in alpha state, and may or may not\n";
    cout << "Function correctly. You hereby agree that its creator can not\n";
    cout << "be held liable in case anything goes wrong.\n";
    cout << "\n";
    cout << "Also, lots of hours of bugfixing and testing went into this.\n";
    cout << "Please show a little compassion or something when using this\n";
    cout << "program :)\n";
    cout << "\n";

    if (argc == < 3)
    {
        cout << "usage: xm2esf <infile> <outfile>\n";
        cout << "\n";
        cout << "<infile>  is the pointer to an XIF file. An XIF file is a descriptor\n";
        cout << "          file that contains all the parameters which xm2esf will\n";
        cout << "          respect while creating the resulting ESF file, and also the\n";
        cout << "          path and filename to the input XM file.\n";
        cout << "\n";
        cout << "          Creating an XIF file is possible using 'xm2esfgui' by\n";
        cout << "          Oerg866. It is very easy to use and creates a XIF file\n";
        cout << "          that is perfectly parseable by xm2esf :)\n";
        cout << "\n";
        cout << "          For other software and information, please visit\n";
        cout << "          http://echo.mdscene.net/software\n";
        cout << "\n";
        cout << "<outfile> is the output path ad filename xm2esf will use to create\n";
        cout << "          the resulting ESF file. Please note that if this file exists\n";
        cout << "          it WILL BE OVERWRITTEN, even if you encounter a bug and the\n";
        cout << "          conversion somehow fails (well.. it shouldn't :P but just\n";
        cout << "          keep in mind to back up often while xm2esf is in alpha stage\n";
        cout << "\n";
        return 2;
    }


    /** ///////////////////////////
    // Initialize variables      //
    /////////////////////////// **/

    // XM to ESF Channel assignment variables

    uint8_t xmfm[7];        // Contains the corresponding XM channel for each FM Channel
    uint8_t xmpsg[4];       // Contains the corresponding XM channel for each PSG channel

    uint8_t fm;             // Amount of FM channels
    uint8_t psg;            // Amount of PSG channels
    uint8_t present[12];    // Used to define if a channel is present

    // XM to ESF Instrument assignment variables

    uint8_t esfins[257];    /* Contains the corresponding Echo instrument ID (in Pointer List)
                            // for each XM instrument */

    // XM to ESF Song adjustment variables

    uint8_t pitch[12];      // Transpose value for each channel
    uint8_t vol[12];        // Channel volume
    double quotient[12];    // Volume conversion quotient (to limit volume to a certain value)

    // XM to ESF Conversion loop and temporary variables

    uint32_t i;             // For some loops
    uint32_t currow;        // Current row of entire song that's being processed

    // XM to ESF Stream data bridge variables

    string row[12] = "     ";   // Current XM row for each channel
    uint8_t curins[12];     // Current XM instrument
    uint8_t curnote[12];    // Current note (XM note with transpose already applied !)
    uint8_t curvol[12];     // Current volume
    uint32_t curfreq[12];   // Current frequency (Used for note slide effects, like 1xx, 2xx, 3xx, 4xx

    uint8_t effectdat[12];  // Current effect ID
    uint8_t effectval[12];  // Current effect data

    uint8_t xmins;          // Instrument column in current XM row

    double slidestep[12];   // Current step during a slide. unit: XM notes

    uint8_t slidetarget[12];// Target for a slide. Unit: XM notes
    uint8_t slidespeed[12]; // Amount of XM notes to slide per tick. Ain't super accurate, but it'll do the job

    double volslidepos[12]; // Current position in volume slide (Effect 0x0A), unit: XM volume...erm.. whatever
    double volslidespeed[12];   // Amount of volume to slide per tick. AFAIK, this is 100% accurate to XM

    uint8_t arpnote1[12];   // Note 1 in arpeggio
    uint8_t arpnote2[12];   // Note 2 in arpeggio

    double vibstep[12];     /* Current step in a vibrato (note this is NOT in XM notes!
                               It's just the amouont of ticks that already have been
                               processed in a vibrato! it starts at 0, so that a smooth
                               sinewave can be applied, that starts at the current note!) */

    uint8_t vibspeed[12];   // Vibrato speed (1st parameter in effect 0x04)
    uint8_t vibdepth[12];   // Vibrato depth (2nd parameter in effect 0x04)

    double conversion;      // Note to frequency conversion variable

    // XM to ESF conversion init variables

    uint8_t esfchan[12];    // Very interesting and important

    esfchan[1] = 0;         // The echo stream format definitions use direct YM2612/PSG
    esfchan[2] = 1;         // channel values. These are not 0-10 or something like that
    esfchan[3] = 2;         // but have gaps in them due to the way the chip is constructed.
    esfchan[4] = 4;         // This bridge variable takes care of this.
    esfchan[5] = 5;
    esfchan[6] = 6;
    esfchan[7] = 8;
    esfchan[8] = 9;
    esfchan[9] = 10;
    esfchan[10] = 12;
    esfchan[11] = 11;

    uint16_t fmnote[12];    // Pre-defined fm frequencies for every note (source: echo table)

    fmnote[0] = 644;
    fmnote[1] = 681;
    fmnote[2] = 722;
    fmnote[3] = 765;
    fmnote[4] = 810;
    fmnote[5] = 858;
    fmnote[6] = 910;
    fmnote[7] = 964;
    fmnote[8] = 1024;
    fmnote[9] = 1081;
    fmnote[10] = 1146;
    fmnote[11] = 1214;

    uint16_t psgnote[12];   // Pre-defined PSG frequencies for every note (source: echo table)

    uint8_t ctype[12];      // Channel type (defines internally which channels from 1 to 11 are FM, PSG, PCM and Noise

/////////////////////////////////////////////////////////
// INIT CODE BEGINS HERE :)
/*
    ' Please note: If the XM's Tempo is set to 150, every tick is exactly the same length as one echo tick
    ' Meaning that the adjustable variable is the "Ticks per row". If XM's tempo is 150, you should set
    ' the ticks per row value as the speed in the XIF input file. If you do that, your XM will be
    ' exactly as fast as the echo output!!

    ' most tempo values other than 150 will mean that the actual esf's speed will only be able to approximate
    ' the actual XM speed.
*/

//  ' This is the XIF file parser. It is long, andthe coding style is probably pretty bad. But! it works :P
//  ' The error handler is not very intelligent yet. But, it'll be, sooner or later. Probably later =P

    ifstream config;        // Configuration input file
    config.open (argv[2], ios::in);

    string setting;
    string xm;

    uint8_t filetype = 1;
    uint8_t esfloop = 1;

    uint8_t tempo = 7;

    while (strlwr(setting)) != "[instruments]")
    {
        getline(config, setting);
        if (setting.substr(0,1) != "#")
        {
            switch (spleft(setting))
            {
            case "FILE":
                xm = param(setting, 1);
                cout << "XM File: " << xm;
                break;
            case "TYPE":
                switch (param(setting, 1))
                {
                case "BGM":
                    filetype = 1;
                    break;
                case "SFX"
                        filetype = 2;
                    break;
                }
                switch (param(setting, 2))
                {
                case "LOOP":
                    esfloop = 1;
                    if (filetype == 2)
                    {
                        cout << "Input file errorneously declares loop while being a SFX. File rejected!";
                        config close;
                        return 1;
                    }
                    break;
                case "NOLOOP":
                    esfloop = 0;
                    break;
                }
                break;
            case "TEMPO":
                tempo = strtoul(param(setting, 1));
                break;
            case "FM":
                fm = strtoul(param(setting, 1));
                if (fm > 6) {
                    cout << "ERROR: Declared more than 6 FM channels.";
                    config close;
                    return 1
                }
                break;
            case "PSG":
                psg = strtoul(param(setting, 1));
                if (psg > 3) {
                    cout << "ERROR: Declared more than 3 PSG channels.":
                    config close;
                    return 1
                }
                break;
            case "PCM":
                pcm = 1;
                if (fm = 6) {
                    cout << "ERROR: Cannot have 6 FM channels and PCM too."
                    config close;
                    return 1
                }
                break:
            case "NOISE"
                noise = 1;
                break;

            case "FM1":
                xmfm[1] = strtoul(param(setting, 1));
                break;
            case "FM2":
                xmfm[2] = strtoul(param(setting, 1));
                break;
            case "FM3":
                xmfm[3] = strtoul(param(setting, 1));
                break;
            case "FM4":
                xmfm[4] = strtoul(param(setting, 1));
                break;
            case "FM5":
                xmfm[5] = strtoul(param(setting, 1));
                break;
            case "FM6":
                xmfm[6] = strtoul(param(setting, 1));
                break;
            case "PCMC":
                xmpcm = strtoul(param(setting,1));
                break;
            case "PSG1":
                xmpsg[1] = strtoul(param(setting, 1));
                break;
            case "PSG2":
                xmpsg[2] = strtoul(param(setting, 1));
                break;
            case "PSG3":
                xmpsg[3] = strtoul(param(setting, 1));
                break;
            case "PSGN":
                xmnoise& = strtoul(param(setting, 1));
                break;
            case "NOISEFREQ":
                noisetype& = strtoul(param(setting, 1));
                break;
            case "NOISETYPE":
                noisemode& = strtoul(param(setting, 1));
                break;
            }
        }
    }

    cout << "Loading XM file: " << xm;

    #if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__TOS_WIN__) || defined(__WINDOWS__)
        system("loadxm \"" + xm + "\"");
    #elif defined (__UNIX__)
        xm.replace(s.begin(), s.end(), ' ', '\ ');
        system("loadxm " + xm);
    #endif

    while (strlwr(setting) != "[volume]" {
            getline(config,setting);
            if (setting.substr(0,1) != "#") {
                switch (spleft(setting)) {
                    case "FM1":
                        pitch[1] = strtoul(param(setting, 1));
                        break;
                    case "FM2":
                        pitch[2] = strtoul(param(setting, 1));
                        break;
                    case "FM3":
                        pitch[3] = strtoul(param(setting, 1));
                        break;
                    case "FM4":
                        pitch[4] = strtoul(param(setting, 1));
                        break;
                    case "FM5":
                        pitch[5] = strtoul(param(setting, 1));
                        break;
                    case "FM6":
                        pitch[6] = strtoul(param(setting, 1));
                        break;

                    case "PSG1":
                        pitch[7] = strtoul(param(setting, 1));
                        break;
                    case "PSG2":
                        pitch[8] = strtoul(param(setting, 1));
                        break;
                    case "PSG3":
                        pitch[9] = strtoul(param(setting, 1));
                        break;
                    case "PSGN":
                        pitch[11] = strtoul(param(setting, 1));
                        break;

                }
            }


    }

    while (strlwr(setting) != "[end]") {
        getline(config, setting);
        if (setting.substr(0,1) != "#") {
            switch (spleft(setting)) {
                case "FM1":
                    vol[1] = strtoul(param(setting,1));
                        break;
                case "FM2":
                    vol[2] = strtoul(param(setting,1));
                        break;
                case "FM3":
                    vol[3] = strtoul(param(setting,1));
                        break;
                case "FM4":
                    vol[4] = strtoul(param(setting,1));
                        break;
                case "FM5":
                    vol[5] = strtoul(param(setting,1));
                        break;
                case "FM6":
                    vol[6] = strtoul(param(setting,1));
                        break;

                case "PSG1":
                    vol[7] = strtoul(param(setting,1));
                        break;
                case "PSG2":
                    vol[8] = strtoul(param(setting,1));
                        break;
                case "PSG3":
                    vol[9] = strtoul(param(setting,1));
                        break;
                case "PSGN":
                    vol[11] = strtoul(param(setting,1));
                        break;

            }
        }

    }

    config close;

    ifstream xif[12];

    for (i=1; i <= fm; i++) {
        present[i] = 1;
        xif[i].open (std::string("temp\\C") + xmfm[i], ios::in | ios::binary);
    }

    for (i=7; i <= (psg + 6); i++) {
        present[i] = 1;
        xif[i].open (std::string("temp\\C") + xmpsg[i-6], ios::in | ios::binary);
    }

    if (pcm = 1) {
        present[10] = 1;
        xif[10].open (std::string("temp\\C") + xmpcm, ios::in | ios::binary);
    }

    if (noise = 1) {
        present[11] = 1;
        xif[11].open (std::string("temp\\C") + xmnoise, ios::in | ios::binary);
    }

//'''''''''''''''''''''''''''''''''''''''
// LOAD ALL PREDEFINED PSG FREQUENCIES ''
//'''''''''''''''''''''''''''''''''''''''

    ifstream psgfreqs;

    psgfreqs.open (std::string("psg.txt"), ios::in);



    return 0;
}
