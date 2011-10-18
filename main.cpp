#include <iostream>
#include <string>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <stdint.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdlib.h>


using namespace std;

int main(int argc, char *argv[])
{
    string version = "0.99.0/c-wip";

    double pi = 3.14159265358979323846264338;

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

    if (argc == < 3) {
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

    uint8_t xmfm[7];
    uint8_t xmpsg[4];

    uint8_t fm;
    uint8_t psg;
    uint8_t present[12];

    uint8_t esfins[257];

    uint8_t pitch[12];
    uint8_t vol[12];
    double quotient[12];

    uint32_t i;
    uint32_t currow;

    string row[12] = "     ";
    uint8_t curins[12];
    uint8_t curnote[12];
    uint8_t curvol[12];
    uint32_t curfreq[12];

    uint8_t effectdat[12];
    uint8_t effectval[12];

    uint8_t xmins;

    double slidestep[12];

    uint8_t slidetarget[12];
    uint8_t slidespeed[12];

    double volslidepos[12];
    double volslidespeed[12];

    uint8_t arpnote1[12];
    uint8_t arpnote2[12];

    double vibstep[12];

    uint8_t vibspeed[12];
    uint8_t vibdepth[12];

    double conversion;

    uint8_t esfchan[12];

    esfchan[1] = 0;
    esfchan[2] = 1;
    esfchan[3] = 2;
    esfchan[4] = 4;
    esfchan[5] = 5;
    esfchan[6] = 6;
    esfchan[7] = 8;
    esfchan[8] = 9;
    esfchan[9] = 10;
    esfchan[10] = 12;
    esfchan[11] = 11;

    uint16_t fmnote[12];

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
    fmnote[11] = 644;

    uint16_t psgnote[12];

    uint8_t ctype[12];

    unlink(argv[2]);

    FILE *config = fopen(argv[1], "r");

    //

    return 0;
}
