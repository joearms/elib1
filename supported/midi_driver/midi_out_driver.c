#include <CoreMIDI/CoreMIDI.h> 
#include <unistd.h>            
#define MESSAGESIZE 3          

typedef unsigned char byte;

void playPacketListOnAllDevices   (MIDIPortRef     midiout, 
                                   const MIDIPacketList* pktlist);


int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

byte out[128];

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

int main(void) 
{
  byte buff[100];
  int idev, n;
  
  // Prepare MIDI Interface Client/Port for writing MIDI data:
  MIDIClientRef midiclient  = NULL;
  MIDIPortRef   midiout     = NULL;
  MIDIEndpointRef dest;

  OSStatus status;
  
  if (status = MIDIClientCreate(CFSTR("TeStInG"), NULL, NULL, &midiclient)) {
    fprintf(stderr,"Error trying to create MIDI Client structure: %d\n", status);
    fprintf(stderr,"%s\n", GetMacOSStatusErrorString(status));
    exit(status);
  }
  if (status = MIDIOutputPortCreate(midiclient, CFSTR("OuTpUt"), &midiout)) {
    fprintf(stderr, "Error trying to create MIDI output port: %d\n", status);
    fprintf(stderr, "%s\n", GetMacOSStatusErrorString(status));
    exit(status);
  }
  
  // find the output device
  read_cmd(buff);
  idev = buff[0];
  // fprintf(stderr, "Use device %d\r\n",idev);
  dest = MIDIGetDestination(idev);
  send_string("ready");
  while (n = read_cmd(buff), n > 0) {
    handle(midiout, dest, n, buff);
  };
  
  sleep(1);
  exit(1);
  
}

handle(MIDIPortRef midiout, MIDIEndpointRef dest, int n, byte* in){

  // fprintf(stderr,"playsome %d\r\n",n);
  // Prepare a MIDI Note-On message to send 
  MIDITimeStamp timestamp = 0;   // 0 will mean play now. 
  Byte buffer[1024];             // storage space for MIDI Packets (max 65536)
  MIDIPacketList *packetlist = (MIDIPacketList*)buffer;
  MIDIPacket *currentpacket = MIDIPacketListInit(packetlist);
  currentpacket = MIDIPacketListAdd(packetlist, sizeof(buffer), 
				    currentpacket, timestamp, n, in);
   MIDISend(midiout, dest, packetlist);
}
