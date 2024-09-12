/* main.c - this file is part of DeSmuME
 *
 * Copyright (C) 2006-2015 DeSmuME Team
 * Copyright (C) 2007 Pascal Giard (evilynux)
 * Used under fair use by the DSonPSP team, 2019
 *
 * This file is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This file is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <psppower.h>
#include <pspdebug.h>
#include <pspctrl.h>
#include <pspdisplay.h>
#include <pspsuspend.h>
#include <pspkernel.h>
#include <psprtc.h>

#include"pspdmac.h"

#include "PSP/FrontEnd.h"
#include "PSP/video.h"

#include "NDSSystem.h"
#include "GPU.h"
#include "SPU.h"
#include "sndsdl.h"
#include "sndpsp.h"
#include "ctrlssdl.h"
#include "slot2.h"

#include "render3D.h"
#include "rasterize.h"

#include <unistd.h>
#include "dirent.h"
#include "PSP/vram.h"
#include "PSP/PSPDisplay.h"

#ifdef PROFILE
#include "pspprof.h"
#endif


#include "PSP/pspvfpu.h"

PSP_MODULE_INFO("DesmuME PSP", 0, 3, 0);

PSP_MAIN_THREAD_ATTR(PSP_THREAD_ATTR_USER | PSP_THREAD_ATTR_VFPU);


//From Daedalus
extern "C" {

	void _DisableFPUExceptions();

}

int RAMAMOUNT()
{
	int iStep = 1024;
	int iAmount = 0;
	short shSalir = 0;

	char* pchAux = NULL;

	while (!shSalir)
	{
		iAmount += iStep;
		pchAux = (char*)malloc(iAmount);
		if (pchAux == NULL)
		{
			//No hay memoria libre!!! = There is no free memory!
			iAmount -= iStep;
			shSalir = 1;

		}
		else
		{
			free(pchAux);
			pchAux = NULL;
		}
	}

	return iAmount;
}

char rom_filename[256];
int FPS_Counter = 0;

volatile bool execute = false;

SoundInterface_struct *SNDCoreList[] = {
  &SNDDummy,
  &SNDPSP,
  NULL
};

GPU3DInterface *core3DList[] = {
  &gpu3DNull,
  &gpu3DRasterize,
//  &gpu3DGU,
  NULL
};

const char * save_type_names[] = {
  "Autodetect",
  "EEPROM 4kbit",
  "EEPROM 64kbit",
  "EEPROM 512kbit",
  "FRAM 256kbit",
  "FLASH 2mbit",
  "FLASH 4mbit",
  NULL
};

configured_features my_config;


void ShowFPS(int x,int y){
  pspDebugScreenSetXY(x,y);
  pspDebugScreenPrintf("FPS: %d        ", FPS_Counter);
}

void PrintfXY(const char* text, int x, int y) {
	pspDebugScreenSetXY(x, y);
	pspDebugScreenPrintf(text);
}



static void desmume_cycle()
{
	 u16 pad = 0;

	 process_ctrls_event(pad);

    /* Update mouse position and click */
	  if (!mouse.click)
	  {
      NDS_releaseTouch();
	  }
	  else {
		  NDS_setTouchPos(mouse.x, mouse.y);
	  }

    update_keypad(pad);     /* Update keypad */

  if (my_config.showfps)
      ShowFPS(0,3);

  

	/*if (my_config.enable_sound)
		SPU_Emulate_user();*/
}


void vdDejaLog(char *msg)
{
    FILE *fd;
    fd = fopen("ZZLOG.TXT", "a");
    fprintf(fd, "%s\n", msg);
    fclose(fd);
}

void WriteLog(char* msg)
{
	FILE* fd;
	fd = fopen("debug_log.txt", "a");
	fprintf(fd, "%s\n", msg);
	fclose(fd);
}

void WriteHash(char* msg)
{
	FILE* fd;
	fd = fopen("hashes.txt", "a");
	fprintf(fd, "%s\n", msg);
	fclose(fd);
}

bool audio_inited = false;

#define PSP_AUDIO_SAMPLE_MAX  8192

void EMU_Conf(){

  //ReAddress display to 32bit vram
  pspDebugScreenInitEx((void*)(0x44000000), PSP_DISPLAY_PIXEL_FORMAT_5551, 1);

  DoConfig(&my_config);

  NDS_3D_ChangeCore(my_config.Render3D);
  backup_setManualBackupType(my_config.savetype);

  pspDebugScreenClear();

  if (my_config.enable_sound && !audio_inited) {
	  SPU_ChangeSoundCore(SNDCORE_PSP, 735 * 4);
	  SPU_SetSynchMode(0, 0 /*CommonSettings.SPU_sync_method*/);
	  audio_inited = true;
  }
  else if (audio_inited && !my_config.enable_sound){
	  SPU_ChangeSoundCore(SNDCORE_DUMMY, 0);
	  audio_inited = false;
  }

  PrintfXY("ROM: ", 0, 1);
  PrintfXY(gameInfo.ROMname, 5, 1);

  char number;
  sprintf(&number, "%1d", my_config.frameskip);
  PrintfXY("Frameskip: ", 55, 1);
  PrintfXY(&number, 65, 1);
}

void ChangeRom(bool reset){

  if (reset){
    NDS_Reset();

    #ifdef PROFILE
      gprof_cleanup();
    #endif

    //Init psp display again
    pspDebugScreenInitEx((void*)(0x44000000), PSP_DISPLAY_PIXEL_FORMAT_5551, 1);
    Init_PSP_DISPLAY_FRAMEBUFF();
  }

  DSEmuGui("",rom_filename);
  pspDebugScreenClear();

  int error = NDS_LoadROM(rom_filename);

  if (error < 0) {
     vdDejaLog("ERROR ROM:");
     vdDejaLog(rom_filename);
	   exit(-1);
  }

  execute = true;
}

void ResetRom() {

	NDS_Reset();
	pspDebugScreenClear();

	if (NDS_LoadROM(rom_filename) < 0) {
		exit(-1);
	}

	execute = true;
}

const int TARGET_FPS = 60;
const int FRAME_TIME_MICROSEC = 1000000 / TARGET_FPS;
const int SINGLE_FRAME_TIME_MICROSEC = FRAME_TIME_MICROSEC / TARGET_FPS;

int main(int argc, char **argv) {

  u32 last_fps_timing = 0;
  u32 fps_base_tick = 0;
  u32 fps_frame_counter = 0;

  /* the firmware settings */
  struct NDS_fw_config_data fw_config;

  scePowerSetClockFrequency(333, 333, 166);

  _DisableFPUExceptions();

  pspDebugScreenInitEx((void*)(0x44000000), PSP_DISPLAY_PIXEL_FORMAT_5551, 1);


  Init_PSP_DISPLAY_FRAMEBUFF();

  NDS_Init();

  /* default the firmware settings, they may get changed later */
  NDS_FillDefaultFirmwareConfigData(&fw_config);

  slot2_Init();

  slot2_Change(NDS_SLOT2_NONE);

  /* Create the dummy firmware */
  NDS_CreateDummyFirmware( &fw_config);

  #ifdef PROFILE
    execute = true;
    NDS_LoadROM("test.nds");
    my_config.Render3D = true;
    my_config.showfps = true;
  #else
    ChangeRom(false);
    EMU_Conf();
  #endif

  extern void test_jit_func();
  test_jit_func();

  EMU_SCREEN(false, false);

  printf("Ram: %d\n", RAMAMOUNT());

  while(execute)
  {
      
    for ( int i = 0; i < my_config.frameskip; i++ ) {
        NDS_SkipNextFrame();
        NDS_exec<false>();
    }

    desmume_cycle();
    NDS_exec<false>();

    u32 curr_timing = sceKernelGetSystemTimeLow();
    fps_frame_counter += 1 + my_config.frameskip;

    if (my_config.fps_cap) {
        // Calcolo del tempo target per il frame attuale
        u32 frame_time = 1000000 / TARGET_FPS;  // Tempo per frame in microsecondi
        u32 target_timing = fps_base_tick + fps_frame_counter * frame_time;
        int delay = target_timing - curr_timing;

        // Se il ritardo è troppo grande o troppo piccolo, resettare il timing di base
        if (delay < -2000  || delay > 50000 ) {
            fps_base_tick = curr_timing;
            fps_frame_counter = 0;  // Resetta il contatore dei frame
        }
        // Applica un ritardo se necessario
        else if (delay > 0) {
            sceKernelDelayThread(delay);
        }
    }


    if(curr_timing - last_fps_timing >= 1000000)
    {
      FPS_Counter = fps_frame_counter;//(fps_frame_counter * 1000000) / (curr_timing - last_fps_timing);
      fps_frame_counter = 0;
      last_fps_timing = curr_timing;
    }
  }

  NDS_DeInit();
  return 0;
}