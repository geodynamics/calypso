!>@file   read_control_elements.f90
!!@brief  module read_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2005
!
!>@brief  Subroutines to read control data
!!
!!@verbatim
!!      subroutine read_real_ctl_item                                   &
!!     &         (c_buf, label, iflag_dat, real_data)
!!      subroutine read_integer_ctl_item                                &
!!     &         (c_buf, label, iflag_dat, int_data)
!!      subroutine read_character_ctl_item                              &
!!     &         (c_buf, label, iflag_dat, chara_data)
!!
!!      subroutine read_real2_ctl_item                                  &
!!     &         (c_buf, label, iflag_dat, real1, real2)
!!      subroutine read_real3_ctl_item                                  &
!!     &         (c_buf, label, iflag_dat, real1, real2, real3)
!!      subroutine read_integer2_ctl_item                               &
!!     &         (c_buf, label, iflag_dat, int1, int2)
!!      subroutine read_integer3_ctl_item                               &
!!     &         (c_buf, label, iflag_dat, int1, int2, int3)
!!      subroutine read_character2_ctl_item                             &
!!     &         (c_buf, label, iflag_dat, chara1, chara2)
!!      subroutine read_character3_ctl_item                             &
!!     &         (c_buf, label, iflag_dat, chara1, chara2, chara3)
!!      subroutine read_charreal2_ctl_item                              &
!!     &         (c_buf, label, iflag_dat, chara, vec1, vec2)
!!      subroutine read_char2real_ctl_item                              &
!!     &         (c_buf, label, iflag_dat, chara1, chara2, vect)
!!      subroutine read_charareal_ctl_item                              &
!!     &         (c_buf, label, iflag_dat, chara, vect)
!!      subroutine read_charaint_ctl_item                               &
!!     &         (c_buf, label, iflag_dat, chara, ivec)
!!      subroutine read_intchrreal_ctl_item                             &
!!     &         (c_buf, label, iflag_dat, ivec, chara, vect)
!!      subroutine read_intreal_ctl_item                                &
!!     &         (c_buf, label, iflag_dat, ivec, vect)
!!      subroutine read_int2real_ctl_item                               &
!!     &         (c_buf, label, iflag_dat, int1, int2, vect)
!!      subroutine read_int2real2_ctl_item                              &
!!     &         (c_buf, label, iflag_dat, int1, int2, vec1, vec2)
!!      subroutine read_charaine3_ctl_item                              &
!!     &         (c_buf, label, iflag_dat, chara, ivec1, ivec2, ivec3)
!!@endverbatim
!!
!!@n @param  label      label for control items
!!@n @param  iflag_end  integer flag for reading block
!!@n @param  iflag_dat  integer flag for reading block
!!@n @param  num_array  size of array block
!!@n @param  num        number of blocks already read
!!@n @param  icou       counter for reading array
!!
!!@n @param real_data     read real data
!!@n @param int_data      read integre data
!!@n @param chara_data    read character data
!!
!!@n @param ivect         integer array data
!!@n @param int1          integer array data
!!@n @param int2          integer array data
!!@n @param vect          real array data
!!@n @param vec1          real array data
!!@n @param vec2          real array data
!!@n @param vec3          real array data
!!@n @param c_tbl         character array data
!!@n @param c1_tbl         character array data
!!@n @param c2_tbl         character array data
!!@n @param c3_tbl         character array data
!
      module read_control_elements
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_real_ctl_item                                     &
     &         (c_buf, label, iflag_dat, real_data)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      real(kind = kreal), intent(inout) :: real_data
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, real_data
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf%header_chara),     &
     &                       real_data
      iflag_dat = 1
!
      end subroutine read_real_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_integer_ctl_item                                  &
     &         (c_buf, label, iflag_dat, int_data)
!
      type(buffer_for_control), intent(in)  :: c_buf
       character(len=kchara), intent(in) :: label
       integer (kind=kint), intent(inout) :: iflag_dat
       integer (kind=kint), intent(inout) :: int_data
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int_data
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf%header_chara),     &
     &                        int_data
      iflag_dat = 1
!
       end subroutine read_integer_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_character_ctl_item                                &
     &         (c_buf, label, iflag_dat, chara_data)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara_data
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara_data
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf%header_chara),     &
     &                       ': ', trim(chara_data)
      iflag_dat = 1
!
      end subroutine read_character_ctl_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_real2_ctl_item                                    &
     &         (c_buf, label, iflag_dat, real1, real2)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      real(kind = kreal), intent(inout) :: real1, real2
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, real1, real2
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &            trim(c_buf%header_chara), ': ', real1, real2
      iflag_dat = 1
!
       end subroutine read_real2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_real3_ctl_item                                    &
     &         (c_buf, label, iflag_dat, real1, real2, real3)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      real(kind = kreal), intent(inout) :: real1, real2, real3
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, real1, real2, real3
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &            trim(c_buf%header_chara), ': ', real1, real2, real3
      iflag_dat = 1
!
      end subroutine read_real3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_integer2_ctl_item                                 &
     &         (c_buf, label, iflag_dat, int1, int2)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      integer (kind=kint), intent(inout) :: int1, int2
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int1, int2
      if (iflag_debug .gt. 0)  write(*,'(a,a2,2i6)')                    &
     &            trim(c_buf%header_chara), ': ', int1, int2
      iflag_dat = 1
!
      end subroutine read_integer2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_integer3_ctl_item                                 &
     &         (c_buf, label, iflag_dat, int1, int2, int3)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      integer (kind=kint), intent(inout) :: int1, int2, int3
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int1, int2, int3
      if (iflag_debug .gt. 0)  write(*,'(a,a2,3i6)')                    &
     &            trim(c_buf%header_chara), ': ', int1, int2, int3
      iflag_dat = 1
!
      end subroutine read_integer3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_character2_ctl_item                               &
     &         (c_buf, label, iflag_dat, chara1, chara2)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara1, chara2
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara1, chara2
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' 1: ', chara1
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' 2: ', chara2
      iflag_dat = 1
!
       end subroutine read_character2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_character3_ctl_item                               &
     &         (c_buf, label, iflag_dat, chara1, chara2, chara3)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara1, chara2, chara3
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara1, chara2, chara3
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' 1: ', chara1
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' 2: ', chara2
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' 3: ', chara3
      iflag_dat = 1
!
       end subroutine read_character3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_charreal2_ctl_item                                &
     &         (c_buf, label, iflag_dat, chara, vec1, vec2)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara
      real(kind = kreal), intent(inout) :: vec1, vec2
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara, vec1, vec2
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' chara: ', chara
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1p2e23.15)')              &
     &            trim(c_buf%header_chara), ' real: ', vec1, vec2
      iflag_dat = 1
!
       end subroutine read_charreal2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_char2real_ctl_item                                &
     &         (c_buf, label, iflag_dat, chara1, chara2, vect)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara1, chara2
      real(kind = kreal), intent(inout) :: vect
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara1, chara2, vect
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' 1: ', chara1
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' 2: ', chara2
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &            trim(c_buf%header_chara), ' real: ', vect
      iflag_dat = 1
!
       end subroutine read_char2real_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_charareal_ctl_item                                &
     &         (c_buf, label, iflag_dat, chara, vect)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara
      real(kind = kreal), intent(inout) :: vect
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara, vect
      if (iflag_debug .gt. 0)  write(*,'(a,a7,a)')                      &
     &            trim(c_buf%header_chara), ' char: ', chara
      if (iflag_debug .gt. 0)  write(*,'(a,a4,1pe23.15)')               &
     &            trim(c_buf%header_chara), ' vect: ', vect
      iflag_dat = 1
!
       end subroutine read_charareal_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_charaint_ctl_item                                 &
     &         (c_buf, label, iflag_dat, chara, ivec)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara
      integer(kind = kint), intent(inout) :: ivec
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara, ivec
      if (iflag_debug .gt. 0)  write(*,'(a,a7,a)')                      &
     &            trim(c_buf%header_chara), ' char: ', chara
      if (iflag_debug .gt. 0)  write(*,'(a,a7,i16)')                    &
     &            trim(c_buf%header_chara), ' int:  ', ivec
      iflag_dat = 1
!
       end subroutine read_charaint_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_intchrreal_ctl_item                               &
     &         (c_buf, label, iflag_dat, ivec, chara, vect)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_dat
      integer(kind = kint), intent(inout) :: ivec
      character(len=kchara), intent(inout) :: chara
      real(kind = kreal), intent(inout) :: vect
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, ivec, chara, vect
      if (iflag_debug .gt. 0)  write(*,'(a,a7,i16)')                    &
     &            trim(c_buf%header_chara), ' int:  ', ivec
      if (iflag_debug .gt. 0)  write(*,'(a,a7,a)')                      &
     &            trim(c_buf%header_chara), ' char: ', chara
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &            trim(c_buf%header_chara), ' real: ', vect
      iflag_dat = 1
!
       end subroutine read_intchrreal_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_intreal_ctl_item                                  &
     &         (c_buf, label, iflag_dat, ivec, vect)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_dat
      integer(kind = kint), intent(inout) :: ivec
      real(kind = kreal), intent(inout) :: vect
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, ivec, vect
      if (iflag_debug .gt. 0)  write(*,'(a,a7,i16)')                    &
     &            trim(c_buf%header_chara), ' int:  ', ivec
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &            trim(c_buf%header_chara), ' real: ', vect
      iflag_dat = 1
!
       end subroutine read_intreal_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_int2real_ctl_item                                 &
     &         (c_buf, label, iflag_dat, int1, int2, vect)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_dat
      integer(kind = kint), intent(inout) :: int1, int2
      real(kind = kreal), intent(inout) :: vect
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int1, int2, vect
      if (iflag_debug .gt. 0)  write(*,'(a,a7,2i16)')                   &
     &            trim(c_buf%header_chara), ' int:  ', int1, int2
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &            trim(c_buf%header_chara), ' real: ', vect
      iflag_dat = 1
!
       end subroutine read_int2real_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_int2real2_ctl_item                                &
     &         (c_buf, label, iflag_dat, int1, int2, vec1, vec2)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_dat
      integer(kind = kint), intent(inout) :: int1, int2
      real(kind = kreal), intent(inout) :: vec1, vec2
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int1, int2, vec1, vec2
      if (iflag_debug .gt. 0)  write(*,'(a,a7,2i16)')                   &
     &            trim(c_buf%header_chara), ' int:  ', int1, int2
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1p2e23.15)')              &
     &            trim(c_buf%header_chara), ' real: ', vec1, vec2
      iflag_dat = 1
!
       end subroutine read_int2real2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_charaine3_ctl_item                                &
     &         (c_buf, label, iflag_dat, chara, ivec1, ivec2, ivec3)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara
      integer(kind = kint), intent(inout) :: ivec1, ivec2, ivec3
!
       character(len=kchara) :: tmpchara
!
!
      if(iflag_dat.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara, ivec1, ivec2, ivec3
      if (iflag_debug .gt. 0)  write(*,'(a,a7,2i16)')                   &
     &         trim(c_buf%header_chara), ' chara:  ', trim(chara)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1p2e23.15)')              &
     &         trim(c_buf%header_chara), ' int: ', ivec1, ivec2, ivec3
      iflag_dat = 1
!
       end subroutine read_charaine3_ctl_item
!
!   --------------------------------------------------------------------
!
      end module read_control_elements
