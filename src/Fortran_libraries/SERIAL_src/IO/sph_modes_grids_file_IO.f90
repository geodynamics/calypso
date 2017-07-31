!>@file   sph_modes_grids_file_IO.f90
!!@brief  module sph_modes_grids_file_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief ASCII spectr data IO routines
!!
!!@verbatim
!!      subroutine read_geom_rtp_file                                   &
!!     &         (file_name, id_file, my_rank_IO, sph_file)
!!      subroutine read_spectr_modes_rj_file                            &
!!     &         (file_name, my_rank_IO, sph_file)
!!      subroutine read_geom_rtm_file                                   &
!!     &         (file_name, id_file, my_rank_IO, sph_file)
!!      subroutine read_modes_rlm_file                                  &
!!     &         (file_name, id_file, my_rank_IO, sph_file)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!
!!      subroutine write_geom_rtp_file                                  &
!!     &         (file_name, id_file, my_rank_IO, sph_file)
!!      subroutine write_spectr_modes_rj_file                           &
!!     &         (file_name, my_rank_IO, sph_file)
!!      subroutine write_geom_rtm_file                                  &
!!     &         (file_name, id_file, my_rank_IO, sph_file)
!!      subroutine write_modes_rlm_file                                 &
!!     &         (file_name, id_file, my_rank_IO, sph_file)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!@endverbatim
!!
!!@param my_rank_IO    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_modes_grids_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_data_IO
      use sph_modes_grids_data_IO
!
      implicit none
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_file                                     &
     &         (file_name, id_file, my_rank_IO, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &     'Read ascii grid file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call read_geom_rtp_data(id_file, my_rank_IO,                      &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO, ierr)
      close(id_file)
!
      end subroutine read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_file                              &
     &         (file_name, id_file, my_rank_IO, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &     'Read ascii spectr modes file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call read_spectr_modes_rj_data(id_file, my_rank_IO,               &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO, ierr)
      close(id_file)
!
      end subroutine read_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_file                                     &
     &         (file_name, id_file, my_rank_IO, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &     'Read ascii grid file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call read_geom_rtm_data(id_file, my_rank_IO,                      &
     &    sph_file%comm_IO, sph_file%sph_IO, ierr)
      close(id_file)
!
      end subroutine read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine read_modes_rlm_file                                    &
     &         (file_name, id_file, my_rank_IO, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &     'Read ascii spectr modes file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call read_spectr_modes_rlm_data(id_file, my_rank_IO,              &
     &    sph_file%comm_IO, sph_file%sph_IO, ierr)
!
      close(id_file)
!
      end subroutine read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_file                                    &
     &         (file_name, id_file, my_rank_IO, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &     'Write ascii grid file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call write_geom_rtp_data(id_file, my_rank_IO,                     &
     &   sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
      close(id_file)
!
      end subroutine write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_file                             &
     &         (file_name, id_file, my_rank_IO, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &     'Write ascii spectr modes file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call write_spectr_modes_rj_data(id_file, my_rank_IO,              &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
      close(id_file)
!
      end subroutine write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_file                                    &
     &         (file_name, id_file, my_rank_IO, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &     'Write ascii grid file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call write_geom_rtm_data(id_file, my_rank_IO,                     &
     &    sph_file%comm_IO, sph_file%sph_IO)
      close(id_file)
!
      end subroutine write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_file                                   &
     &         (file_name, id_file, my_rank_IO, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: my_rank_IO
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &     'Write ascii spectr modes file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call write_modes_rlm_data(id_file, my_rank_IO,                    &
     &    sph_file%comm_IO, sph_file%sph_IO)
      close(id_file)
!
      end subroutine write_modes_rlm_file
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_file_IO
