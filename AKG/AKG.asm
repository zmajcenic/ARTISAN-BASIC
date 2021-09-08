    org #4012


Main_Player_Start:
        ;Selects the hardware. Mandatory, as CPC is default.
        PLY_AKG_HARDWARE_MSX = 1
        PLY_AKG_MANAGE_SOUND_EFFECTS = 1
        ;Want a ROM player (a player without automodification)?
        ;PLY_AKG_Rom = 1                         ;Must be set BEFORE the player is included.
        
        ;Declares the buffer for the ROM player, if you're using it. You can declare it anywhere of course.
        ;LIMITATION: the SIZE of the buffer (PLY_AKG_ROM_BufferSize) is only known *after* ther player is compiled.
        ;A bit annoying, but you can compile once, get the buffer size, and hardcode it to put the buffer wherever you want.
        ;Note that the size of the buffer shrinks when using the Player Configuration feature. Use the largest size and you'll be safe.
        IFDEF PLY_AKG_Rom
                PLY_AKG_ROM_Buffer = #F975                  ;Can be set anywhere.
        ENDIF


        include "PlayerAkg.asm"
Main_Player_End:


TesterEnd:

        print "Size of player: ", {hex}(Main_Player_End - Main_Player_Start)
        ;print "Size of music: ", {hex}(Music_Crowd_Noise_End - Music_Maintitle_Start)
        ;print "Size of sfx: ", {hex}(SoundEffects_End - SoundEffects_Start)
        ;        IFDEF PLY_AKG_ROM
        ;print "Size of buffer in ROM: ", {hex}(PLY_AKG_ROM_BufferSize)
        ;        ENDIF
        ;print "Total size (player and music): ", {hex}($ - Music_Maintitle_Start)