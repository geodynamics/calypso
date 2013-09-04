!
!      module m_bc_data_list
!
!      Written by H. Matsui on Jan., 2009
!
!      subroutine allocate_nod_bc_list_temp
!      subroutine allocate_nod_bc_list_velo
!      subroutine allocate_nod_bc_list_press
!      subroutine allocate_nod_bc_list_vecp
!      subroutine allocate_nod_bc_list_magne
!      subroutine allocate_nod_bc_list_mag_p
!      subroutine allocate_nod_bc_list_j
!      subroutine allocate_nod_bc_list_composit
!
!      subroutine deallocate_nod_bc_list_temp
!      subroutine deallocate_nod_bc_list_velo
!      subroutine deallocate_nod_bc_list_press
!      subroutine deallocate_nod_bc_list_vecp
!      subroutine deallocate_nod_bc_list_magne
!      subroutine deallocate_nod_bc_list_mag_p
!      subroutine deallocate_nod_bc_list_j
!      subroutine deallocate_nod_bc_list_composit
!
      module m_bc_data_list
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint) :: num_bc_e
      real (kind=kreal),      allocatable :: bc_e_magnitude(:)
      integer (kind=kint),    allocatable :: ibc_e_type(:)
      character (len=kchara), allocatable :: bc_e_name(:)
!
!
      integer (kind=kint) :: num_bc_v
      real (kind=kreal),      allocatable :: bc_v_magnitude(:)
      integer (kind=kint),    allocatable :: ibc_v_type(:)
      character (len=kchara), allocatable :: bc_v_name(:)
!
      integer (kind=kint) :: num_bc_p
      real (kind=kreal),      allocatable :: bc_p_magnitude(:)
      integer (kind=kint),    allocatable :: ibc_p_type(:)
      character (len=kchara), allocatable :: bc_p_name(:)
! 
!
      integer (kind=kint) :: num_bc_vp
      real (kind=kreal),      allocatable :: bc_vp_magnitude(:)
      integer (kind=kint),    allocatable :: ibc_vp_type(:)
      character (len=kchara), allocatable :: bc_vp_name(:)
!
      integer (kind=kint) :: num_bc_b
      real (kind=kreal),      allocatable :: bc_b_magnitude(:)
      integer (kind=kint),    allocatable :: ibc_b_type(:)
      character (len=kchara), allocatable :: bc_b_name(:)
!
      integer (kind=kint) :: num_bc_j
      real (kind=kreal),      allocatable :: bc_j_magnitude(:)
      integer (kind=kint),    allocatable :: ibc_j_type(:)
      character (len=kchara), allocatable :: bc_j_name(:)
!
!
      integer (kind=kint) :: num_bc_mag_p
      real (kind=kreal), allocatable :: bc_mag_p_magnitude(:)
      integer (kind=kint), allocatable :: ibc_mag_p_type(:)
      character (len=kchara), allocatable :: bc_mag_p_name(:)
! 
!
      integer (kind=kint) :: num_bc_composit
      real (kind=kreal), allocatable :: bc_composit_magnitude(:)
      integer (kind=kint), allocatable :: ibc_composit_type(:)
      character (len=kchara), allocatable :: bc_composit_name(:)
! 
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_temp
!
        allocate(bc_e_name(num_bc_e))
        allocate(bc_e_magnitude(num_bc_e))
        allocate(ibc_e_type(num_bc_e))
!
        if(num_bc_e .gt. 0) then
          ibc_e_type = 0
          bc_e_magnitude = 0.0d0
        end if
!
      end subroutine allocate_nod_bc_list_temp
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_velo
!
        allocate(bc_v_name(num_bc_v))
        allocate(bc_v_magnitude(num_bc_v))
        allocate(ibc_v_type(num_bc_v))
!
        if(num_bc_v .gt. 0) then
          ibc_v_type = 0
          bc_v_magnitude = 0.0d0
        end if
!
      end subroutine allocate_nod_bc_list_velo
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_press
!
        allocate(bc_p_name(num_bc_p))
        allocate(bc_p_magnitude(num_bc_p))
        allocate(ibc_p_type(num_bc_p))
!
        if(num_bc_p .gt. 0) then
          ibc_p_type = 0
          bc_p_magnitude = 0.0d0
        end if
!
      end subroutine allocate_nod_bc_list_press
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_vecp
!
        allocate(bc_vp_name(num_bc_vp))
        allocate(bc_vp_magnitude(num_bc_vp))
        allocate(ibc_vp_type(num_bc_vp))
!
        if(num_bc_vp .gt. 0) then
          ibc_vp_type = 0
          bc_vp_magnitude = 0.0d0
        end if
!
      end subroutine allocate_nod_bc_list_vecp
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_magne
!
        allocate(bc_b_name(num_bc_b))
        allocate(bc_b_magnitude(num_bc_b))
        allocate(ibc_b_type(num_bc_b))
!
        if(num_bc_b .gt. 0) then
          ibc_b_type = 0
          bc_b_magnitude = 0.0d0
        end if
!
      end subroutine allocate_nod_bc_list_magne
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_j
!
        allocate(bc_j_name(num_bc_j))
        allocate(bc_j_magnitude(num_bc_j))
        allocate(ibc_j_type(num_bc_j))
!
        if(num_bc_j .gt. 0) then
          ibc_j_type = 0
          bc_j_magnitude = 0.0d0
        end if
!
      end subroutine allocate_nod_bc_list_j
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_mag_p
!
        allocate(bc_mag_p_name(num_bc_mag_p))
        allocate(bc_mag_p_magnitude(num_bc_mag_p))
        allocate(ibc_mag_p_type(num_bc_mag_p))
!
        if(num_bc_mag_p .gt. 0) then
          ibc_mag_p_type = 0
          bc_mag_p_magnitude = 0.0d0
        end if
!
      end subroutine allocate_nod_bc_list_mag_p
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_bc_list_composit
!
        allocate(bc_composit_magnitude(num_bc_composit))
        allocate(ibc_composit_type(num_bc_composit))
        allocate(bc_composit_name(num_bc_composit))
!
        if(num_bc_composit .gt. 0) then
          ibc_composit_type = 0
          bc_composit_magnitude = 0.0d0
        end if
!
      end subroutine allocate_nod_bc_list_composit
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_temp
!
        deallocate(bc_e_name)
        deallocate(bc_e_magnitude)
        deallocate(ibc_e_type)
!
      end subroutine deallocate_nod_bc_list_temp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_velo
!
        deallocate(bc_v_name)
        deallocate(bc_v_magnitude)
        deallocate(ibc_v_type)
!
      end subroutine deallocate_nod_bc_list_velo
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_press
!
        deallocate(bc_p_name)
        deallocate(bc_p_magnitude)
        deallocate(ibc_p_type)
!
      end subroutine deallocate_nod_bc_list_press
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_vecp
!
        deallocate(bc_vp_name)
        deallocate(bc_vp_magnitude)
        deallocate(ibc_vp_type)
!
      end subroutine deallocate_nod_bc_list_vecp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_magne
!
        deallocate(bc_b_name)
        deallocate(bc_b_magnitude)
        deallocate(ibc_b_type)
!
      end subroutine deallocate_nod_bc_list_magne
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_mag_p
!
        deallocate(bc_mag_p_name)
        deallocate(bc_mag_p_magnitude)
        deallocate(ibc_mag_p_type)
!
      end subroutine deallocate_nod_bc_list_mag_p
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_j
!
        deallocate(bc_j_name)
        deallocate(bc_j_magnitude)
        deallocate(ibc_j_type)
!
      end subroutine deallocate_nod_bc_list_j
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_bc_list_composit
!
        deallocate(bc_composit_magnitude)
        deallocate(ibc_composit_type)
        deallocate(bc_composit_name)
!
      end subroutine deallocate_nod_bc_list_composit
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_list
