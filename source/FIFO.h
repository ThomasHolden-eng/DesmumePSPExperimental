/*
	Copyright 2006 yopyop
	Copyright 2007 shash
	Copyright 2007-2011 DeSmuME team
	Copyright 2016-2022 melonDS team

	This file is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	This file is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with the this software.  If not, see <http://www.gnu.org/licenses/>.

    Some of this file is part of melonDS.

    melonDS is free software: you can redistribute it and/or modify it under
    the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    melonDS is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with melonDS. If not, see http://www.gnu.org/licenses/.
*/


#ifndef FIFO_H
#define FIFO_H

#include <string.h>

#include "types.h"

//=================================================== IPC FIFO

template<typename T, u32 NumEntries>
class FIFO
{
public:
    void Clear()
    {
        NumOccupied = 0;
        ReadPos = 0;
        WritePos = 0;
        memset(&Entries[ReadPos], 0, sizeof(T));
    }


    void Write(T val)
    {
        if (IsFull()) return;

        Entries[WritePos] = val;

        WritePos++;
        if (WritePos >= NumEntries)
            WritePos = 0;

        NumOccupied++;
    }

    T Read()
    {
        T ret = Entries[ReadPos];
        if (IsEmpty())
            return ret;

        ReadPos++;
        if (ReadPos >= NumEntries)
            ReadPos = 0;

        NumOccupied--;
        return ret;
    }

    T Peek()
    {
        return Entries[ReadPos];
    }

    T Peek(u32 offset)
    {
        u32 pos = ReadPos + offset;
        if (pos >= NumEntries)
            pos -= NumEntries;

        return Entries[pos];
    }

    u32 Level() { return NumOccupied; }
    bool IsEmpty() { return NumOccupied == 0; }
    bool IsFull() { return NumOccupied >= NumEntries; }

    bool CanFit(u32 num) { return ((NumOccupied + num) <= NumEntries); }

    T Entries[NumEntries] = {0};
    u32 NumOccupied = 0;
    u32 ReadPos = 0, WritePos = 0;
};

extern FIFO<u32, 16> IPCFIFO9; // ARM9 write FIFO
extern FIFO<u32, 16> IPCFIFO7; // ARM7 write FIFO
extern void IPC_FIFOinit(u8 proc);
extern void IPC_FIFOsend(u8 proc, u32 val);
extern u32 IPC_FIFOrecv(u8 proc);
extern void IPC_FIFOcnt(u8 proc, u16 val);

//=================================================== GFX FIFO

//yeah, its oversize for now. thats a simpler solution
//moon seems to overdrive the fifo with immediate dmas
//i think this might be nintendo code too
#define HACK_GXIFO_SIZE 200000

typedef struct
{
	u8		cmd[HACK_GXIFO_SIZE];
	u32		param[HACK_GXIFO_SIZE];

	u32		head;		// start position
	u32		tail;		// tail
	u32		size;		// size FIFO buffer
	u32		matrix_stack_op_size; //number of matrix stack items in the fifo (stack is busy when this is nonzero)
} GFX_FIFO;

typedef struct
{
	u8		cmd[4];
	u32		param[4];

	u8		head;
	u8		tail;
	u8		size;
} GFX_PIPE;

extern GFX_PIPE gxPIPE;
extern GFX_FIFO gxFIFO;
extern void GFX_PIPEclear();
extern void GFX_FIFOclear();
extern void GFX_FIFOsend(u8 cmd, u32 param);
extern BOOL GFX_PIPErecv(u8 *cmd, u32 *param);
extern void GFX_FIFOcnt(u32 val);

//=================================================== Display memory FIFO
typedef struct
{
	u32		buf[0x6000];			// 256x192 32K color
	u32		head;					// head
	u32		tail;					// tail
} DISP_FIFO;

extern DISP_FIFO disp_fifo;
extern void DISP_FIFOinit();
extern void DISP_FIFOsend(u32 val);
extern u32 DISP_FIFOrecv();

#endif
