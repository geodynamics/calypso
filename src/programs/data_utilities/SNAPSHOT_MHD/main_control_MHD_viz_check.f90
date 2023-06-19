!>@file   main_control_MHD_viz_check.f90
!!@brief  program kemorin_control_MHD_check
!!
!!@author H. Matsui
!!@date Programmed by by H. Matsui in July 2023
!
!>@brief  Main program to check control file for SPH_MHD
!!         with visualizers
!!         Input ontrol file: control_snapshot
!
      program control_MHD_w_viz_check
!
      use m_precision
!
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_vizs
      use write_control_elements
!
      implicit none
!
!>      File name for control file
      character(len=kchara) :: MHD_ctl_name
      character(len=kchara), parameter                                  &
     &                    :: hd_mhd_ctl = 'MHD_control'
!
      type(mhd_simulation_control) :: MHD_ctl1
      type(add_vizs_sph_mhd_ctl) :: add_VMHD_ctl1
      type(buffer_for_control) :: c_buf1
!
!
      if(iargc_kemo() .le. 0) then
        write(*,*) 'check_control_mhd CONTROL_FILE_NAME'
        stop
      end if
      call getarg_k(1, MHD_ctl_name)
!
      c_buf1%level = 0
      call read_control_4_sph_MHD_w_vizs(MHD_ctl_name,                  &
     &                                MHD_ctl1, add_VMHD_ctl1, c_buf1)
      if(c_buf1%iend .gt. 0) stop 'Error in control file'
!
!
      write(id_monitor,'(a)') '!  '
      write(id_monitor,'(a)') '!  Checked control data'
      write(id_monitor,'(a)') '!  '
      call write_sph_mhd_ctl_w_vizs(id_monitor, hd_mhd_ctl,             &
     &    MHD_ctl1, add_VMHD_ctl1, c_buf1%level)
!
      stop '***** program finished *****'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine getarg_k(i, argc)
!
      integer, intent(in) :: i
      character(len=*), intent(out) :: argc
!
      call getarg(0, argc)
      if(argc == "") then
        call getarg(i + 1, argc)
      else
        call getarg(i, argc)
      end if
      end subroutine getarg_k
!
!   --------------------------------------------------------------------
!
      integer function iargc_kemo() result(oresult)
!
      integer :: iargc
      character(len=8) :: argc
      oresult = iargc()
      call getarg(0, argc)
      if(argc == "") then
        oresult = oresult - 1
      end if
      end function iargc_kemo
!
!   --------------------------------------------------------------------
!
      end program control_MHD_w_viz_check
