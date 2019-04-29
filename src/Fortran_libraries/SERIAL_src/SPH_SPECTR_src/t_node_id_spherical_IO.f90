!> @file  t_node_id_spherical_IO.f90
!!      module t_node_id_spherical_IO
!!
!! @author  H. Matsui
!! @date Written in July, 2007
!
!> @brief Array for speherical harmonics indexing IO
!!
!!@verbatim
!!      subroutine dealloc_sph_grid_idx_IO(sph_IO)
!!      subroutine dealloc_sph_mode_idx_IO(sph_IO)
!!
!!      subroutine alloc_num_idx_sph_IO(sph_IO)
!!      subroutine alloc_nod_id_sph_IO(sph_IO)
!!      subroutine alloc_idx_sph_1d1_IO(sph_IO)
!!      subroutine alloc_idx_sph_1d2_IO(sph_IO)
!!      subroutine alloc_idx_sph_1d3_IO(sph_IO)
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module t_node_id_spherical_IO
!
      use m_precision
!
      implicit none
!
!
!>      Structure for spherical harmonics table IO
      type sph_IO_data
!>        Subdomain ID in each direction
        integer(kind = kint) :: sph_rank(3) = (/0,0,0/)
!
!>        Number of global components in each direction
        integer(kind = kint) :: nidx_gl_sph(3) = (/0,0,0/)
!>        Truncation of spherical hermonis
        integer(kind = kint) :: ltr_gl
!
!>        Number of direction of data points
        integer(kind = kint) :: numdir_sph
!>        Number of data points
        integer(kind = kint) :: numnod_sph
!
!>        Global node ID
        integer(kind = kint_gl), allocatable :: inod_gl_sph(:)
!>        Global radial and mode index
        integer(kind = kint), allocatable :: idx_gl_sph(:,:)
!
!>        Number of local compoennts in each direction
        integer(kind = kint), allocatable :: nidx_sph(:)
!>        Start global address
        integer(kind = kint), allocatable :: ist_sph(:)
!>        End global address
        integer(kind = kint), allocatable :: ied_sph(:)
!>        Number of components for global address
        integer(kind = kint) :: ncomp_table_1d(3)
!
!>        First global index for specttr data
        integer(kind = kint), allocatable :: idx_gl_1(:)
!>        Second global index for specttr data
        integer(kind = kint), allocatable :: idx_gl_2(:,:)
!>        Third global index for specttr data
        integer(kind = kint), allocatable :: idx_gl_3(:,:)
!
!>        global radial data
        real(kind = kreal), allocatable :: r_gl_1(:)
      end type sph_IO_data
!
      private :: dealloc_num_idx_sph_IO, dealloc_idx_sph_1d1_IO
      private :: dealloc_idx_sph_1d2_IO, dealloc_idx_sph_1d3_IO
      private :: dealloc_nod_id_sph_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_grid_idx_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
      call dealloc_idx_sph_1d3_IO(sph_IO)
      call dealloc_nod_id_sph_IO(sph_IO)
!
      end subroutine dealloc_sph_grid_idx_IO
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_mode_idx_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
      call dealloc_nod_id_sph_IO(sph_IO)
!
      end subroutine dealloc_sph_mode_idx_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_num_idx_sph_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      allocate(sph_IO%nidx_sph(sph_IO%numdir_sph))
      allocate(sph_IO%ist_sph(sph_IO%numdir_sph))
      allocate(sph_IO%ied_sph(sph_IO%numdir_sph))
!
      sph_IO%nidx_sph = 0
      sph_IO%ist_sph = 0
      sph_IO%ied_sph =  0
!
      end subroutine alloc_num_idx_sph_IO
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_id_sph_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      allocate(sph_IO%inod_gl_sph(sph_IO%numnod_sph))
      allocate(sph_IO%idx_gl_sph(sph_IO%numnod_sph,sph_IO%numdir_sph))
!
      if(sph_IO%numnod_sph .le. 0) return
      sph_IO%inod_gl_sph = 0
      sph_IO%idx_gl_sph =  0
!
      end subroutine alloc_nod_id_sph_IO
!
! -----------------------------------------------------------------------
!
      subroutine alloc_idx_sph_1d1_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      allocate( sph_IO%idx_gl_1(sph_IO%nidx_sph(1)) )
      allocate( sph_IO%r_gl_1(sph_IO%nidx_sph(1)) )
!
      if(sph_IO%nidx_sph(1) .le. 0) return
      sph_IO%idx_gl_1 =  0
      sph_IO%r_gl_1 = 0.0d0
!
      end subroutine alloc_idx_sph_1d1_IO
!
! -----------------------------------------------------------------------
!
      subroutine alloc_idx_sph_1d2_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: n1, n2
!
!
      n1 = sph_IO%nidx_sph(2)
      n2 = sph_IO%ncomp_table_1d(2)
      allocate( sph_IO%idx_gl_2(n1,n2) )
      if(n1*n2 .gt. 0) sph_IO%idx_gl_2 =  0
!
      end subroutine alloc_idx_sph_1d2_IO
!
! -----------------------------------------------------------------------
!
      subroutine alloc_idx_sph_1d3_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: n1, n2
!
!
      n1 = sph_IO%nidx_sph(3)
      n2 = sph_IO%ncomp_table_1d(3)
      allocate( sph_IO%idx_gl_3(n1,n2) )
      if(n1*n2 .gt. 0) sph_IO%idx_gl_3 =  0
!
      end subroutine alloc_idx_sph_1d3_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_idx_sph_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      deallocate( sph_IO%nidx_sph )
      deallocate( sph_IO%ist_sph )
      deallocate( sph_IO%ied_sph )
!
      end subroutine dealloc_num_idx_sph_IO
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_id_sph_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      deallocate( sph_IO%inod_gl_sph )
      deallocate( sph_IO%idx_gl_sph  )
!
      end subroutine dealloc_nod_id_sph_IO
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_idx_sph_1d1_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      deallocate( sph_IO%idx_gl_1 )
      deallocate( sph_IO%r_gl_1 )
!
      end subroutine dealloc_idx_sph_1d1_IO
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_idx_sph_1d2_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      deallocate( sph_IO%idx_gl_2 )
!
      end subroutine dealloc_idx_sph_1d2_IO
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_idx_sph_1d3_IO(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      deallocate( sph_IO%idx_gl_3 )
!
      end subroutine dealloc_idx_sph_1d3_IO
!
! -----------------------------------------------------------------------
!
      end module t_node_id_spherical_IO
