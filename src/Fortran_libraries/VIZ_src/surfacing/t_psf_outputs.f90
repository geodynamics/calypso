!>@file   t_psf_outputs.f90
!!@brief  module t_psf_outputs
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on July, 2006
!!@n       Modified by H.Matsui on March, 2013
!
!>@brief Output data for parallel surface rendering
!!
!!@verbatim
!!      subroutine alloc_psf_outputs_num(nprocs, num_psf, psf_out)
!!      subroutine alloc_psf_outputs_data(psf_out)
!!      subroutine alloc_SR_array_psf(my_rank, max_ncomp_psf,           &
!!     &          nnod_psf_tot, npatch_tot)
!!
!!      subroutine dealloc_psf_outputs_num
!!      subroutine dealloc_psf_outputs_data(my_rank, num_psf)
!!      subroutine dealloc_SR_array_psf(my_rank)
!!@endverbatim
!!
!!@param my_rank       subdomain ID
!!@param nprocs        number of total processes
!!@param num_psf       number of sections
!!@param max_ncomp_psf    maximum number of components for sections
!!@param nnod_psf_tot         total number of node for sections
!!@param npatch_tot   total number of elements for sections
!
      module t_psf_outputs
!
      use m_precision
      use m_geometry_constants
!
      implicit  none
!
      type psf_collect_type
        integer(kind = kint) :: ntot_nod_output_psf = 0
        integer(kind = kint), pointer :: istack_nod_output_psf(:)
!
        integer(kind = kint), pointer :: istack_nod_para_psf(:)
        integer(kind = kint), pointer :: istack_nod_recv_psf(:)
!
        integer(kind = kint) :: ntot_ele_output_psf = 0
        integer(kind = kint), pointer :: istack_ele_output_psf(:)
!
        integer(kind = kint), pointer :: istack_ele_para_psf(:)
        integer(kind = kint), pointer :: istack_ele_recv_psf(:)
!
        integer(kind = kint), pointer :: ihash_output_psf(:)
!
        real(kind = kreal), pointer :: send_psf(:)
        real(kind = kreal), pointer :: recv_psf(:)
        integer(kind = kint), pointer :: isend_psf(:)
        integer(kind = kint), pointer :: irecv_psf(:)
      end type psf_collect_type
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_psf_outputs_num(nprocs, num_psf, psf_out)
!
      integer(kind = kint), intent(in) :: num_psf, nprocs
      type(psf_collect_type), intent(inout) :: psf_out
!
!
      allocate( psf_out%istack_nod_output_psf(0:num_psf) )
      allocate( psf_out%istack_nod_para_psf(0:num_psf*nprocs) )
      allocate( psf_out%istack_nod_recv_psf(0:num_psf*nprocs) )
!
      allocate( psf_out%istack_ele_output_psf(0:num_psf) )
      allocate( psf_out%istack_ele_para_psf(0:num_psf*nprocs) )
      allocate( psf_out%istack_ele_recv_psf(0:num_psf*nprocs) )
!
      psf_out%istack_nod_output_psf = 0
      psf_out%istack_nod_para_psf = 0
      psf_out%istack_nod_recv_psf = 0
!
      psf_out%istack_ele_output_psf = 0
      psf_out%istack_ele_para_psf = 0
      psf_out%istack_ele_recv_psf = 0
!
      end subroutine alloc_psf_outputs_num
!
! ----------------------------------------------------------------------
!
      subroutine alloc_psf_outputs_data(psf_out)
!
      type(psf_collect_type), intent(inout) :: psf_out
!
!
      allocate(psf_out%ihash_output_psf(psf_out%ntot_nod_output_psf) )
      if(psf_out%ntot_nod_output_psf.gt.0) psf_out%ihash_output_psf = 0
!
      end subroutine alloc_psf_outputs_data
!
! ----------------------------------------------------------------------
!
      subroutine alloc_SR_array_psf(my_rank, max_ncomp_psf,             &
     &          nnod_psf_tot, npatch_tot, psf_out)
!
      integer(kind=kint ) , intent(in)   ::  my_rank
      integer(kind=kint ) , intent(in)   ::  max_ncomp_psf
      integer(kind=kint ) , intent(in)   ::  nnod_psf_tot
      integer(kind=kint ) , intent(in)   ::  npatch_tot
      type(psf_collect_type), intent(inout) :: psf_out
!
      integer(kind = kint) :: nmax_comp, nmax_int, num
!
!
      nmax_comp = max(max_ncomp_psf,num_triangle)
      allocate (psf_out%send_psf(nmax_comp*nnod_psf_tot))
      if(nmax_comp*nnod_psf_tot .gt. 0) psf_out%send_psf = 0.0d0
!
      nmax_int = max(num_triangle*npatch_tot,nnod_psf_tot)
      allocate (psf_out%isend_psf(nmax_int))
      if(nmax_int .gt. 0) psf_out%isend_psf = 0
!
      if (my_rank.eq.0) then
        num = nmax_comp*psf_out%ntot_nod_output_psf
        allocate(psf_out%recv_psf(num))
        if(psf_out%ntot_nod_output_psf .gt. 0) psf_out%recv_psf = 0.0d0
!
        nmax_int = max(num_triangle*psf_out%ntot_ele_output_psf,        &
     &             psf_out%ntot_nod_output_psf)
        allocate (psf_out%irecv_psf(nmax_int))
        if(nmax_int .gt. 0) psf_out%irecv_psf = 0
      end if
!
      end subroutine alloc_SR_array_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_psf_outputs_num(psf_out)
!
      type(psf_collect_type), intent(inout) :: psf_out
!
      deallocate( psf_out%istack_nod_output_psf)
      deallocate( psf_out%istack_nod_para_psf)
!
      deallocate( psf_out%istack_ele_output_psf)
      deallocate( psf_out%istack_ele_para_psf)
!
      end subroutine dealloc_psf_outputs_num
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_psf_outputs_data(psf_out)
!
      type(psf_collect_type), intent(inout) :: psf_out
!
!
      deallocate(psf_out%ihash_output_psf)
!
      end subroutine dealloc_psf_outputs_data
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_SR_array_psf(my_rank, psf_out)
!
      integer(kind=kint ) , intent(in)   ::  my_rank
      type(psf_collect_type), intent(inout) :: psf_out
!
      deallocate(psf_out%send_psf, psf_out%isend_psf)
!
      if(my_rank.eq.0) deallocate (psf_out%recv_psf, psf_out%irecv_psf)
!
      end subroutine dealloc_SR_array_psf
!
! ----------------------------------------------------------------------
!
      end module t_psf_outputs
