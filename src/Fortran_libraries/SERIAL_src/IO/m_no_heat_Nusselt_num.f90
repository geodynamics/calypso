!>@file   m_no_heat_Nusselt_num.f90
!!@brief  module m_no_heat_Nusselt_num
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for Nusselt number
!!
!!@verbatim
!!      subroutine write_no_heat_source_Nu(idx_rj_degree_zero,          &
!!     &          i_step, time)
!!
!!      subroutine open_read_no_heat_source_Nu(id_pick)
!!      subroutine read_no_heat_source_Nu(id_pick, i_step, time, ierr)
!!@endverbatim
!
      module m_no_heat_Nusselt_num
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>      Output flag for Nusselt number IO
      integer(kind = kint) :: iflag_no_source_Nu = 0
!>      File ID for Nusselt number IO
      integer(kind = kint), parameter :: id_Nusselt = 23
!>      File prefix for Nusselt number file
      character(len = kchara) :: Nusselt_file_head = 'Nusselt'
!
!>      Radius at inner boundary
      real(kind = kreal) :: r_ICB_Nu
!>      Radius at outer boundary
      real(kind = kreal) :: r_CMB_Nu
!>      Nusselt number at inner boundary
      real(kind = kreal) :: Nu_ICB
!>      Nusselt number at outer boundary
      real(kind = kreal) :: Nu_CMB
!
      private :: open_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_no_heat_source_Nu
!
      use set_parallel_file_name
      use write_field_labels
!
      character(len = kchara) :: Nusselt_file_name
!
!
      call add_dat_extension(Nusselt_file_head, Nusselt_file_name)
      open(id_Nusselt, file = Nusselt_file_name,                        &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_Nusselt, file = Nusselt_file_name,                        &
     &    form='formatted', status='replace')
!
!
      write(id_Nusselt,'(a)')    '# Inner_radius, Outer_radius'
      write(id_Nusselt,'(1p2e25.15e3)') r_ICB_Nu, r_CMB_Nu
!
      write(id_Nusselt,'(a)',advance='NO')                              &
     &    't_step    time    Nu_ICB    Nu_CMB'
      write(id_Nusselt,'(a)') ''
!
      end subroutine open_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      subroutine write_no_heat_source_Nu(idx_rj_degree_zero,            &
     &          i_step, time)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero, i_step
      real(kind = kreal), intent(in) :: time
!
!
      if(iflag_no_source_Nu .eq. izero) return
      if(idx_rj_degree_zero .eq. izero) return
!
      call open_no_heat_source_Nu
!
      write(id_Nusselt,'(i10,1p3e23.14e3)')                             &
     &       i_step, time, Nu_ICB, Nu_CMB
!
      close(id_Nusselt)
!
      end subroutine write_no_heat_source_Nu
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_read_no_heat_source_Nu(id_pick)
!
      use skip_comment_f
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: id_pick
      character(len = kchara) :: Nusselt_file_name
!
      integer(kind = kint) :: i
      character(len=255) :: tmpchara
!
!
      call add_dat_extension(Nusselt_file_head, Nusselt_file_name)
      open(id_pick, file = Nusselt_file_name)
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) r_ICB_Nu, r_CMB_Nu
!
      read(id_pick,*) (tmpchara,i=1,4)
!
      end subroutine open_read_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      subroutine read_no_heat_source_Nu(id_pick, i_step, time, ierr)
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
!
      ierr = 0
      read(id_pick,*,err=99,end=99) i_step, time, Nu_ICB, Nu_CMB
!
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      end module m_no_heat_Nusselt_num
