!>@file   m_pickup_sph_spectr_data.f90
!!@brief  module m_pickup_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring specified modes
!!
!!@verbatim
!!      subroutine allocate_num_pick_layer
!!      subroutine allocate_pick_sph_mode
!!      subroutine allocate_pick_sph_l
!!      subroutine allocate_pick_sph_m
!!      subroutine allocate_pick_sph_monitor
!!
!!      subroutine deallocate_num_pick_layer
!!      subroutine deallocate_pick_sph_mode
!!      subroutine deallocate_pick_sph_monitor
!!
!!      subroutine write_sph_spec_4_monitor(my_rank, i_step, time)
!!
!!      subroutine open_sph_spec_read_monitor(id_pick)
!!      subroutine read_sph_spec_4_monitor(id_pick, i_step, time, ierr)
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module m_pickup_sph_spectr_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>      File ID for spectrum monitor file
      integer(kind = kint), parameter :: id_pick_mode = 22
!>      File prefix for spectr monitoring file
      character(len = kchara) :: pickup_sph_head =  'picked_ene_spec'
!
!>      Number of modes of monitoring spectrum to be evaluated
      integer(kind = kint) :: num_pick_sph = 0
!>      Degree and Order ID of  monitoring spectrum to be evaluated
      integer(kind = kint), allocatable :: idx_pick_sph_mode(:)
!>      Number of degrees of  monitoring spectrum to be evaluated
      integer(kind = kint) :: num_pick_sph_l = 0
!>      Degree ID of  monitoring spectrum to be evaluated
      integer(kind = kint), allocatable :: idx_pick_sph_l(:)
!>      Number of orders of  monitoring spectrum to be evaluated
      integer(kind = kint) :: num_pick_sph_m = 0
!>      Order ID of  monitoring spectrum to be evaluated
      integer(kind = kint), allocatable :: idx_pick_sph_m(:)
!
!>      Number of radial layer for monitoring spectrum
      integer(kind = kint) :: num_pick_layer = 0
!>      Radial ID for monitoring spectrum
      integer(kind = kint), allocatable :: id_pick_layer(:)
!>      Radius for monitoring spectrum
      real(kind = kreal), allocatable :: r_pick_layer(:)
!
!>      Total number of modes of  monitoring spectrum to be evaluated
      integer(kind = kint) :: ntot_pick_sph_mode = 0
!>      Number of modes of  monitoring spectrum to be evaluated
      integer(kind = kint) :: num_pick_sph_mode =  0
!>      Global spherical harmonics ID to evaluate  monitoring spectrum
      integer(kind = kint), allocatable :: idx_pick_sph_gl(:)
!>      Local spherical harmonics ID to evaluate  monitoring spectrum
      integer(kind = kint), allocatable :: idx_pick_sph_lc(:)
!
!>      Number of fields for monitoring output
!!       @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) ::  num_fld_pick_sph =    0
!>      Total number of component for monitoring spectrum
      integer(kind = kint) :: ntot_comp_pick_sph =  0
!>      Number of component for monitoring spectrum
      integer (kind=kint), allocatable :: istack_comp_pick_sph(:)
!>       Field  address for monitoring of @f$ f(r,j) @f$
      integer (kind=kint), allocatable :: ifield_monitor_rj(:)
!>      monitoring spectrum
      real(kind = kreal), allocatable :: d_rj_pick_sph_gl(:,:)
!>      Localy evaluated  monitoring spectrum
      real(kind = kreal), allocatable :: d_rj_pick_sph_lc(:,:)
!>      Name of  monitoring spectrum
      character(len=kchara), allocatable :: pick_sph_spec_name(:)
!
      private :: open_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_pick_layer
!
!
      allocate( id_pick_layer(num_pick_layer) )
      allocate( r_pick_layer(num_pick_layer) )
      if(num_pick_layer .gt. 0) then
        id_pick_layer = 0
        r_pick_layer = 0.0d0
      end if
!
      end subroutine allocate_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_mode
!
      allocate( idx_pick_sph_mode(num_pick_sph) )
      if(num_pick_sph .gt. 0) idx_pick_sph_mode = 0
!
      end subroutine allocate_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_l
!
      allocate( idx_pick_sph_l(num_pick_sph_l) )
      if(num_pick_sph_l .gt. 0) idx_pick_sph_l = 0
!
      end subroutine allocate_pick_sph_l
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_m
!
      allocate( idx_pick_sph_m(num_pick_sph_m) )
      if(num_pick_sph_m .gt. 0) idx_pick_sph_m = 0
!
      end subroutine allocate_pick_sph_m
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_monitor
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
!
      num = ntot_pick_sph_mode*num_pick_layer
!
      allocate( idx_pick_sph_gl(ntot_pick_sph_mode) )
      allocate( idx_pick_sph_lc(ntot_pick_sph_mode) )
      allocate( d_rj_pick_sph_lc(ntot_comp_pick_sph,num) )
      allocate( d_rj_pick_sph_gl(ntot_comp_pick_sph,num) )
      allocate( pick_sph_spec_name(ntot_comp_pick_sph) )
      allocate( istack_comp_pick_sph(0:num_fld_pick_sph) )
      allocate( ifield_monitor_rj(num_fld_pick_sph) )
!
      if(num_fld_pick_sph .gt. 0) then
        ifield_monitor_rj = 0
        istack_comp_pick_sph = 0
      end if
      if(num .gt. 0) then
        idx_pick_sph_gl = -1
        idx_pick_sph_lc =  0
        d_rj_pick_sph_lc = 0.0d0
        d_rj_pick_sph_gl = 0.0d0
      end if
!
      end subroutine allocate_pick_sph_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_pick_layer
!
      deallocate( id_pick_layer, r_pick_layer)
!
      end subroutine deallocate_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_mode
!
!
      deallocate( idx_pick_sph_mode )
      deallocate( idx_pick_sph_l, idx_pick_sph_m )
!
      end subroutine deallocate_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_monitor
!
!
      deallocate(idx_pick_sph_gl, d_rj_pick_sph_gl)
      deallocate(idx_pick_sph_lc, d_rj_pick_sph_lc)
      deallocate(pick_sph_spec_name)
      deallocate(istack_comp_pick_sph, ifield_monitor_rj)
!
      end subroutine deallocate_pick_sph_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_spec_4_monitor
!
      use set_parallel_file_name
      use write_field_labels
!
      character(len = kchara) :: pickup_sph_name
!
!
      call add_dat_extension(pickup_sph_head, pickup_sph_name)
      open(id_pick_mode, file = pickup_sph_name, form='formatted',      &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_pick_mode, file = pickup_sph_name, form='formatted',      &
     &    status='replace')
!
      write(id_pick_mode,'(a)')
      write(id_pick_mode,'(a)')    '# num_layers, num_spectr'
      write(id_pick_mode,'(2i10)') num_pick_layer, num_pick_sph_mode
      write(id_pick_mode,'(a)')    '# number of component'
      write(id_pick_mode,'(i10)') ntot_comp_pick_sph
!
      write(id_pick_mode,'(a)',advance='NO')    't_step    time    '
      write(id_pick_mode,'(a)',advance='NO')    'radius_ID    radius    '
      write(id_pick_mode,'(a)',advance='NO')    'degree    order    '
!
      call write_multi_labels(id_pick_mode, ntot_comp_pick_sph,         &
     &    pick_sph_spec_name)

      write(id_pick_mode,'(a)') ''
!
      end subroutine open_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_spec_4_monitor(my_rank, i_step, time)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: inum, knum, j, l, m, ipick, i_fld
!
!
      if(num_pick_sph_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      call open_sph_spec_4_monitor
!
      do inum = 1, num_pick_sph_mode
        j = idx_pick_sph_gl(inum)
        l = int( aint(sqrt(dble(j))) )
        m = j - l*(l+1)
        do knum = 1, num_pick_layer
          ipick = knum + (inum-1) * num_pick_layer
          write(id_pick_mode,'(i10,1pe23.14e3)', advance='NO')          &
     &               i_step, time
          write(id_pick_mode,'(i10,1pe23.14e3,2i10)', advance='NO')     &
     &               id_pick_layer(knum), r_pick_layer(knum), l, m
          do i_fld = 1, ntot_comp_pick_sph
            write(id_pick_mode,'(1pe23.14e3)', advance='NO')            &
     &              d_rj_pick_sph_gl(i_fld,ipick)
          end do
          write(id_pick_mode,'(a)') ''
        end do
      end do
!
      close(id_pick_mode)
!
      end subroutine write_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_spec_read_monitor(id_pick)
!
      use set_parallel_file_name
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint) :: i
!
      character(len = kchara) :: pickup_sph_name
      character(len=255) :: tmpchara
!
!
      call add_dat_extension(pickup_sph_head, pickup_sph_name)
      open(id_pick, file = pickup_sph_name)
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) num_pick_layer, num_pick_sph_mode
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) ntot_comp_pick_sph
!
      ntot_pick_sph_mode = num_pick_sph_mode
      call allocate_num_pick_layer
      call allocate_pick_sph_monitor
!
      read(id_pick,*) (tmpchara,i=1,6),                                 &
     &                 pick_sph_spec_name(1:ntot_comp_pick_sph)
!
      end subroutine open_sph_spec_read_monitor
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spec_4_monitor(id_pick, i_step, time, ierr)
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      integer(kind = kint) :: inum, knum, l, m, ipick
!
!
      ierr = 0
      do inum = 1, num_pick_sph_mode
        do knum = 1, num_pick_layer
          ipick = knum + (inum-1) * num_pick_layer
          read(id_pick,*,err=99,end=99) i_step, time,                   &
     &               id_pick_layer(knum), r_pick_layer(knum), l, m,     &
     &               d_rj_pick_sph_gl(1:ntot_comp_pick_sph,ipick)
          idx_pick_sph_gl(inum) = l*(l+1) + m
        end do
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      end module m_pickup_sph_spectr_data
