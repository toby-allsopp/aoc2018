#include <stdio.h>

int main() {
    long r[6] = {0};
L0:
r[5] = 123;
L1:
r[5] = (r[5]) & (456);
L2:
r[5] = (r[5]) == (72);
L3:
//goto L(1) + ((r[5]) + (3));
if (r[5]) goto L5; else goto L4;
L4:
goto L1;
L5:
r[5] = 0;
L6:
r[2] = (r[5]) | (65536);
L7:
r[5] = 7571367;
L8:
r[4] = (r[2]) & (255);
L9:
r[5] = (r[5]) + (r[4]);
L10:
r[5] = (r[5]) & (16777215);
L11:
r[5] = (r[5]) * (65899);
L12:
r[5] = (r[5]) & (16777215);
L13:
r[4] = (256) > (r[2]);
L14:
//goto L(1) + ((r[4]) + (14));
if (r[4]) goto L16; else goto L15;
L15:
goto L17;
L16:
goto L28;
L17:
r[4] = 0;
L18:
r[3] = (r[4]) + (1);
L19:
r[3] = (r[3]) * (256);
L20:
r[3] = (r[3]) > (r[2]);
L21:
//goto L(1) + ((r[3]) + (21));
if (r[3]) goto L23; else goto L22;
L22:
goto L24;
L23:
goto L26;
L24:
r[4] = (r[4]) + (1);
L25:
goto L18;
L26:
r[2] = r[4];
L27:
goto L8;
L28:
printf("%ld\n", r[5]);
return 0;
r[4] = (r[5]) == (r[0]);
L29:
//goto L(1) + ((r[4]) + (29));
if (r[4]) goto L31; else goto L30;
L30:
goto L6;
L31:
return 0;
}