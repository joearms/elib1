#include <stdio.h>
#include <unistd.h>
#include <CoreMIDI/CoreMIDI.h>  /* interface to MIDI in Macintosh OS X */
#include <stdio.h>

typedef unsigned char byte;

void   printPacketInfo          (const MIDIPacket* packet);
void   myReadProc               (const MIDIPacketList *packetList,
                                 void* readProcRefCon, void* srcConnRefCon);

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

byte buff[64], out[128];

int read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);

  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

send_string(char *str){
  int n;
  n = strlen(str);
  write_cmd(str, n);
}


int main() {
  int fn, arg1, arg2, result, n;
  byte buff[100];

  // Prepare MIDI Interface Client/Port for writing MIDI data:
  MIDIClientRef midiclient  = NULL;
  MIDIPortRef   midiin     = NULL;
  OSStatus status;

  fprintf(stderr,"starting\r\n");

  if (status = MIDIClientCreate(CFSTR("TeStInG"), NULL, NULL, &midiclient)) {
    fprintf(stderr,"Error trying to create MIDI Client structure: %d\r\n", status);
    fprintf(stderr,"%s\r\n", GetMacOSStatusErrorString(status));
    exit(status);
  }

  if (status = MIDIInputPortCreate(midiclient, CFSTR("InPuT"), myReadProc,
				   NULL, &midiin)) {
    fprintf(stderr,"Error trying to create MIDI output port: %d\r\n", status);
    fprintf(stderr,"%s\r\n", GetMacOSStatusErrorString(status));
    exit(status);
  }

  // wait to be told which device to connect to
  read_cmd(buff);
  ItemCount iSrc;
  iSrc = buff[0];
  fprintf(stderr, "opening keyboard:%d\r\n", iSrc);
  MIDIEndpointRef src;
  src = MIDIGetSource(iSrc);
  MIDIPortConnectSource(midiin, src, NULL);

  CFRunLoopRef runLoop;
  runLoop = CFRunLoopGetCurrent();
  send_string("ready");
  CFRunLoopRun();
  handle();
}

void myReadProc(const MIDIPacketList *packetList, void* readProcRefCon,
		void* srcConnRefCon) {
  MIDIPacket *packet = (MIDIPacket*)packetList->packet;
  int i, j;
  int count = packetList->numPackets;
  for (j=0; j<count; j++) {
    printPacketInfo(packet);
    packet = MIDIPacketNext(packet);
  }
}

void printPacketInfo(const MIDIPacket* packet) {
   double timeinsec = packet->timeStamp / (double)1e9;
   // printf("%9.3lf\t", timeinsec);
   int i;
   for (i=0; i<packet->length; i++) {
     out[i] = packet->data[i];
   };
   write_cmd(out, packet->length);
}

handle(int n, byte* in){
  if (in[0] == 1) {
    // stop
    out[0] = 99;
    write_cmd(out, 1);
    //    exit(1);
  };
}
