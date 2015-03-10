!>@file   range_data_IO.f90
!!@brief  module range_data_IO
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2000
!!@n    Modified by H. Matsui in  Aug., 2007
!
!> @brief DAta output for range of fields
!!@verbatim
!!      subroutine output_range_data(istep_ucd, time)
!!      subroutine skip_range_data
!!@endverbatim
!!
!!@param  istep_ucd  step number for field data output
!
      module range_data_IO
!
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind=kint), parameter :: maximum_data_code =     44
      integer(kind=kint), parameter :: maximum_position_code = 45
! 
      character(len=kchara), parameter                                  &
     &       :: minmax_data_file_name =     'maximum_data.dat'
      character(len=kchara), parameter                                  &
     &       :: minmax_posi_file_name =     'maximum_posi.dat'
!
      private :: maximum_data_code, minmax_data_file_name
      private :: maximum_position_code, minmax_posi_file_name
      private :: open_maximum_file
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine open_maximum_file
!
      use m_node_phys_data
      use m_cal_max_indices
      use write_field_labels
!
!
      open (maximum_data_code,file = minmax_data_file_name,             &
     &      status='old', position='append', err = 99)
      open (maximum_position_code,file = minmax_posi_file_name,         &
     &      status='old', position='append', err = 98)
      return
!
!
  98  continue
      close(maximum_data_code)
  99  continue
!
      open (maximum_data_code,file = minmax_data_file_name,             &
     &     status='replace')
      open (maximum_position_code,file = minmax_posi_file_name,         &
     &     status='replace')
!
      write(maximum_data_code,'(a)',advance='no')                       &
     &    'ID step time x y z     '
      write(maximum_position_code,'(a)',advance='no')                   &
     &    'ID step time x y z     '
!
      call write_multi_labels(maximum_data_code,                        &
     &    num_tot_nod_phys_vis, phys_nod_name)
      call write_multi_labels(maximum_data_code,                        &
     &    num_tot_nod_phys_vis, phys_nod_name)
      call write_multi_labels(maximum_position_code,                    &
     &    num_tot_nod_phys_vis, phys_nod_name)
      call write_multi_labels(maximum_position_code,                    &
     &    num_tot_nod_phys_vis, phys_nod_name)
!
      write(maximum_data_code,'(a)')     ''
      write(maximum_position_code,'(a)') ''
!
      end subroutine open_maximum_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_range_data(istep_ucd, time)
!
      use calypso_mpi
      use m_node_phys_data
      use m_cal_max_indices
      use cal_max_indices
!
      integer(kind = kint), intent(in) :: istep_ucd
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fmt_txt
!
!
      call s_cal_max_indices
!
      if ( my_rank .ne. 0 ) return
!
      call open_maximum_file
!
      write(fmt_txt,'(a5,i3,a13)')                                      &
     &   '(i16,', (itwo*num_tot_nod_phys_vis+ione), '(1pE25.15e3))'
      write(maximum_data_code,fmt_txt) istep_ucd, time,                 &
     &      phys_min(1:num_tot_nod_phys_vis),                           &
     &      phys_max(1:num_tot_nod_phys_vis)
!
      write(fmt_txt,'(a16,i3,a6)')                                      &
     &   '(i16,1pE25.15e3,', (itwo*num_tot_nod_phys_vis), '(i16))'
      write(maximum_position_code,fmt_txt) istep_ucd, time,             &
     &      node_min(1:num_tot_nod_phys_vis),                           &
     &      node_max(1:num_tot_nod_phys_vis)
!
      close (maximum_data_code)
      close (maximum_position_code)
!
      end subroutine output_range_data
!
!  ---------------------------------------------------------------------
!
      subroutine skip_range_data
!
      use calypso_mpi
      use m_node_phys_data
      use m_t_step_parameter
!
      integer (kind = kint) :: iflag, i_read_step, i, itmp
      real(kind = kreal) :: rtmp
!
      if ( my_rank .ne. 0 ) return
!
      iflag = i_step_init - mod(istep_max_dt, i_step_output_ucd)
!
      do
        read(maximum_data_code,*,err=99,end=99)  i_read_step, rtmp,     &
     &      (rtmp,i=1,num_tot_nod_phys_vis),                            &
     &      (rtmp,i=1,num_tot_nod_phys_vis)
          if (i_read_step.ge.iflag) exit
        end do
  99  continue
!
      do
        read(maximum_position_code,*,err=98,end=98)                     &
     &      i_read_step, rtmp, (itmp,i=1,num_tot_nod_phys_vis),         &
     &      (itmp,i=1,num_tot_nod_phys_vis)
        if (i_read_step.ge.iflag) exit
      end do
  98  continue
!
      end subroutine skip_range_data
!
!  ---------------------------------------------------------------------
!
      end module range_data_IO
