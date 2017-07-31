!>@file   t_cal_max_indices.f90
!!@brief  module t_cal_max_indices
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Aug., 2007
!
!>@brief  Find node positions of maximum values
!!
!!@verbatim
!!      subroutine alloc_phys_range(ncomp_viz, range)
!!      subroutine dealloc_phys_range(range)
!!      subroutine cal_max_indices(node, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(in) :: nod_fld
!!@endverbatim
!
      module t_cal_max_indices
!
      use m_precision
      use t_geometry_data
      use t_phys_data
!
      implicit  none
!
!
      type maximum_informations
!>        Number of components for range data
        integer(kind=kint) :: ntot_comp
!>        Global node address for minimum value
        integer(kind=kint_gl), allocatable :: node_min(:)
!>        Global node address for maximum value
        integer(kind=kint_gl), allocatable :: node_max(:)
!>        Minimum value of field
        real(kind=kreal), allocatable :: phys_min(:)
!>        Maximum value of field
        real(kind=kreal), allocatable :: phys_max(:)
!
!>        node address for minimum value in subdomain
        integer(kind=kint_gl), allocatable :: inod_min_lc(:)
!>        node address for minimum value in subdomain
        integer(kind=kint_gl), allocatable :: inod_max_lc(:)
!>        Minimum value of field in subdomain
        real(kind=kreal), allocatable :: phys_min_lc(:)
!>        Maximum value of field in subdomain
        real(kind=kreal), allocatable :: phys_max_lc(:)
      end type maximum_informations
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
      private :: cal_max_indices, open_maximum_file
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine alloc_phys_range(ncomp_viz, range)
!
       integer(kind = kint), intent(in) :: ncomp_viz
       type(maximum_informations), intent(inout) :: range
!
!
       range%ntot_comp = ncomp_viz
       allocate(range%node_min(range%ntot_comp))
       allocate(range%node_max(range%ntot_comp))
       allocate(range%phys_min(range%ntot_comp))
       allocate(range%phys_max(range%ntot_comp))
!
       allocate(range%inod_min_lc(range%ntot_comp))
       allocate(range%inod_max_lc(range%ntot_comp))
       allocate(range%phys_min_lc(range%ntot_comp))
       allocate(range%phys_max_lc(range%ntot_comp))
!
       range%node_min = 0
       range%node_max = 0
       range%phys_min = 0.0d0
       range%phys_max = 0.0d0
!
       range%phys_min_lc = 1.0d15
       range%phys_max_lc =-1.0d15
!
       end subroutine alloc_phys_range
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_phys_range(range)
!
       type(maximum_informations), intent(inout) :: range
!
!
       deallocate(range%node_min, range%node_max)
       deallocate(range%phys_min, range%phys_max)
!
       deallocate (range%phys_min_lc, range%phys_max_lc)
       deallocate (range%inod_min_lc, range%inod_max_lc)
!
       end subroutine dealloc_phys_range
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_range_data(istep_ucd, time, node, nod_fld, range)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: istep_ucd
      real(kind = kreal), intent(in) :: time
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
       type(maximum_informations), intent(inout) :: range
!
      character(len=kchara) :: fmt_txt
!
!
      call cal_max_indices(node, nod_fld, range)
!
      if ( my_rank .ne. 0 ) return
      call open_maximum_file(nod_fld)
!
      write(fmt_txt,'(a5,i3,a13)')                                      &
     &   '(i16,', (itwo*range%ntot_comp+ione), '(1pE25.15e3))'
      write(maximum_data_code,fmt_txt) istep_ucd, time,                 &
     &   range%phys_min(1:range%ntot_comp),                             &
     &   range%phys_max(1:range%ntot_comp)
!
      write(fmt_txt,'(a16,i3,a6)')                                      &
     &   '(i16,1pE25.15e3,', (itwo*range%ntot_comp), '(i16))'
      write(maximum_position_code,fmt_txt) istep_ucd, time,             &
     &   range%node_min(1:range%ntot_comp),                             &
     &   range%node_max(1:range%ntot_comp)
!
      close (maximum_data_code)
      close (maximum_position_code)
!
      end subroutine output_range_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_max_indices(node, nod_fld, range)
!
      use calypso_mpi
      use t_geometry_data
      use t_phys_data
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
      type(maximum_informations), intent(inout) :: range
!
      integer(kind = kint) :: nd, inod_min, inod_max
!
!
!$omp parallel workshare
        range%phys_max_lc = MAXVAL(nod_fld%d_fld,1)
        range%phys_min_lc = MINVAL(nod_fld%d_fld,1)
        range%inod_max_lc = MAXLOC(nod_fld%d_fld,1)
        range%inod_min_lc = MINLOC(nod_fld%d_fld,1)
!$omp end parallel workshare

!$omp parallel do private(nd,inod_min,inod_max)
      do nd = 1, range%ntot_comp
        range%inod_max_lc(nd) = node%inod_global(range%inod_max_lc(nd))
        range%inod_min_lc(nd) = node%inod_global(range%inod_min_lc(nd) )
      end do
!$omp end parallel do
!
      call MPI_allREDUCE (range%phys_max_lc, range%phys_max,            &
     &    range%ntot_comp, CALYPSO_REAL, MPI_MAX,                       &
     &    CALYPSO_COMM, ierr_MPI)
!
      call MPI_allREDUCE (range%phys_min_lc, range%phys_min,                  &
     &    range%ntot_comp, CALYPSO_REAL, MPI_MIN,                       &
     &    CALYPSO_COMM, ierr_MPI)
!
      call MPI_allREDUCE (range%inod_max_lc, range%node_max,            &
     &    range%ntot_comp, CALYPSO_GLOBAL_INT, MPI_SUM,                 &
     &    CALYPSO_COMM, ierr_MPI)
!
      call MPI_allREDUCE (range%inod_min_lc, range%node_min,            &
     &    range%ntot_comp, CALYPSO_GLOBAL_INT, MPI_SUM,                 &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine cal_max_indices
!
!  ---------------------------------------------------------------------
!
      subroutine open_maximum_file(nod_fld)
!
      use write_field_labels
!
      type(phys_data), intent(in) :: nod_fld
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
     &    nod_fld%ntot_phys_viz, nod_fld%phys_name)
      call write_multi_labels(maximum_data_code,                        &
     &    nod_fld%ntot_phys_viz, nod_fld%phys_name)
      call write_multi_labels(maximum_position_code,                    &
     &    nod_fld%ntot_phys_viz, nod_fld%phys_name)
      call write_multi_labels(maximum_position_code,                    &
     &    nod_fld%ntot_phys_viz, nod_fld%phys_name)
!
      write(maximum_data_code,'(a)')     ''
      write(maximum_position_code,'(a)') ''
!
      end subroutine open_maximum_file
!
! ----------------------------------------------------------------------
!
      end module t_cal_max_indices
