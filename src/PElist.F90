! This file is part of the forIGES library 
! https://github.com/rweed/forIGES

! Copyright (C) 2024 Richard Weed.
! All rights reserved.

! Redistribution and use in source and binary forms, with or without 
! modification, are permitted provided that the following conditions are met:

! 1. Redistributions of source code, in whole or in part, must retain the  
! above copyright notice, this list of conditions and the following 
! disclaimer.

! 2. Redistributions in binary form, in whole or in part, must reproduce the 
! above copyright notice, this list of conditions and the following disclaimer 
! in the documentation and/or other materials provided with the distribution.

! 3. The names of the contributors may not be used to endorse or promote from 
! products derived from this software without specific prior written 
! permission.

! 4. Redistribution of this source code, including any modifications, may 
! not be intentionally obfuscated.

! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
! IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
! THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND
! CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
! EXEMPLARARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
! OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
! WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
! OTHERWISE), ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
! ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Module PElist 

!! Implements a circular doubly-linked list using a sentinel (head) node   
!! to store IGES P section entities. The current implementation of PElist
!! is really overkill in terms of the number of list methods. The code for
!! PElist was taken from prior work and I kept all of the methods supported
!! in that project since its easier to remove them when I'm convinced I 
!! don't need them than it is to try to add them later.

!! Author  : Richard Weed

!! Version : 0.1 (Beta release 1)
!! Date    : August 18, 2024

  USE IGES_Psection

  Implicit NONE

  PRIVATE

  Type :: PEnode_t
  !! PE list node container.
    Class(entity_t),   Allocatable :: PE 
    !! entity_t class to hold a polymorphic PE
    Type(PEnode_t), Pointer     :: next_p
    !! pointer to next node
    Type(PEnode_t), Pointer     :: prev_p 
    !! pointer to previous node

  Contains

    Procedure :: next
    !! Returns next node pointer 
    Procedure :: previous
    !! Returns previous node pointer
    Procedure :: replaceNode
    !! replace the current pointed to node
    Procedure :: delete => deleteNodeNext_p
    !! delete the next node pointer

  End Type

  Type :: PElist_t
  !! Derived type for constructing a circular list of P section entities
    Type(PEnode_t), Pointer :: head => NULL()
    !! head or sentinal node
    Integer                 :: count = 0
    !! Counter to keep up with current number of entities in a list

  CONTAINS

    Procedure :: append
    !! Add a PE entity at end of current list 
    Procedure :: prepend
    !! Add a PE entity at start of current list 
    Procedure :: insertBefore
    !! Insert a PE entity before a given list location
    Procedure :: insertAfter
    !! Insert a PE entity after a given list location
    Procedure :: appendNode
    !! Add an entire PEnode_t container at the end of current list 
    Procedure :: prependNode
    !! Add an entire PEnode_t container at the start of current list 
    Procedure :: insertNodeBefore
    !! Add an entire PEnode_t container before a given list location 
    Procedure :: insertNodeAfter
    !! Add an entire PEnode_t container after a given list location 
    Procedure :: removeNode
    !! Remove a PEnode_t container at given list location 
    Procedure :: removeFirst
    !! Remove a the first  PEnode_t container in the list 
    Procedure :: removeLast
    !! Remove a the last  PEnode_t container in the list 
    Procedure :: sentinel
    !! Returns a pointer to the sentinel/head node 
    Procedure :: first
    !! Returns a pointer to the first value in the list 
    Procedure :: last
    !! Returns a pointer to the last value in the list 
    Procedure :: isEmpty
    !! Returns a Logical value thats true if the current list is empty 
    Procedure :: clear 
    !! Cycles through list deleting individual nodes 
    Procedure :: delete
    !! Deletes the entire list 
    Procedure :: init
    !! Initialized/creates a list with either an empty first node or with
    !! a user supplied entity value 
    Procedure :: copyTo
    !! Copies entire list to a new user defined list 
    Procedure :: copyFrom
    !! Copies entire list from a user defined list to current list 
    Procedure :: printList => printPElist 
    !! Prints contents of entire list to a stdout or a user defined I/O unit
    Procedure :: move      => moveToNode
    !! Moved current list pointer by a user supplied number of nodes 
    Procedure :: merge     => mergeLists
    !! Merges two lists either appending to first list or returning a new list 
    Procedure :: size      => listSize 
    !! Returns size of current list 
 
  End Type 

  Interface ASSIGNMENT(=)
    Module Procedure assignPElists
    !! Defines an assignment operator for equating two lists
  End Interface

  Interface PEnode_
    Module Procedure newPEnode
    !! Constructor for a new list node
  End Interface

  Interface deleteNode
    Module Procedure deletePEnodePtr
  End Interface

  Public :: PEnode_t, PElist_t, ASSIGNMENT(=), deleteNode

CONTAINS

  Function newPEnode(PE) Result (Node)
  !! Constructor for a new list node. Returns a pointer

    Class(entity_t),Optional, Intent(IN) :: PE 
    Type(PEnode_t), Pointer              :: node

    Character(200) :: emsg
    Integer :: istat

    ALLOCATE (node)
    If (PRESENT(PE)) Then
      ALLOCATE (node%PE, SOURCE=PE, stat=istat, errmsg=emsg)
    EndIf
    
    node%prev_p => node
    node%next_p => node

  End Function newPEnode
 
  Function next(node)
  !! Returns a pointer to next node in the list

    Class(PEnode_t), Target, Intent(INOUT) :: node
    Type(PEnode_t),  Pointer               :: next

    next => node%next_p

  End Function next

  Function previous(node)
  !! Returns a pointer to the previous node in the list

    Class(PEnode_t), Target, Intent(INOUT) :: node
    Type(PEnode_t),  Pointer               :: previous 

    previous => node%prev_p

  End Function previous

  Subroutine replaceNode(oldnode, newnode)
  !! Replaces a given node with a new user defined node
    
    Class(PEnode_t), Target, Intent(INOUT) :: oldnode  
    Type(PEnode_t),  Target, Intent(INOUT) :: newnode

    Type(PEnode_t), Pointer :: oldprev, oldnext

    oldprev => oldnode%prev_p
    oldnext => oldnode%next_p
    newnode%next_p=>oldnext
    newnode%prev_p=>oldprev

    If (ALLOCATED(oldnode%PE)) DEALLOCATE(oldnode%PE)

  End Subroutine replaceNode

  Subroutine deleteNodeNext_p(node)
  !! Deletes the node pointed to by the curren next pointer

    Class(PEnode_t), Intent(INOUT) :: node
    
    If (ASSOCIATED(node%next_p)) Then
      If (ALLOCATED(node%next_p%PE)) Then
        DEALLOCATE(node%next_p%PE)
      EndIf
      DEALLOCATE(node%next_p)
    EndIf
    node%next_p=>NULL()

  End Subroutine deleteNodeNext_p 

  Subroutine deletePEnodePtr(node)

    Class(PEnode_t), Pointer :: node
    !! Deletes a given node pointer

    If (ASSOCIATED(node)) Then
      Call node%delete()
      DEALLOCATE(node)
    EndIf

  End Subroutine deletePEnodePtr

  Subroutine init(list, PE)
  !! Initializes list as an empty list or fills the first node with the
  !! optional entity_t value (PE)

    Class(PElist_t), Target,   Intent(INOUT) :: list
    Class(entity_t), Optional, Intent(IN)    :: PE 


    If (PRESENT(PE)) Then
      list%head => PEnode_(PE)
    Else
      list%head => PEnode_()
    EndIf

    list%head%next_p => list%head
    list%head%prev_p => list%head

  End Subroutine init 

  Function first(list)
  !! Returns pointer to the first node in the list

    Class(PElist_t),         Intent(IN) :: list
    Type(PEnode_t),  Pointer            :: first

    first => list%head%next_p

  End Function first

  Function last(list)
  !! Returns pointer to the last node in the list
 
    Class(PElist_t),         Intent(IN) :: list
    Type(PEnode_t),  Pointer            :: last 

    last => list%head%prev_p 

  End Function last

  Subroutine insertBefore(list, node, PE)
  !! Inserts a new PE value before a given nodes location in list 

    Class(PElist_t), Target,  Intent(INOUT) :: list 
    Type(PEnode_t),  Pointer, Intent(INOUT) :: node 
    Class(entity_t),          Intent(IN)    :: PE 

    Type(PEnode_t), Pointer :: newnode

    newnode => PEnode_(PE)
    
    newnode%next_p      => node 
    newnode%prev_p      => node%prev_p
    node%prev_p%next_p  => newnode
    node%prev_p         => newnode 
   
    list%count = list%count+1

  End Subroutine insertBefore

  Subroutine insertNodeBefore(list, node, newnode)
  !! Inserts a complete node before a given node location in list 

    Class(PElist_t), Target,  Intent(INOUT) :: list 
    Type(PEnode_t),  Pointer, Intent(INOUT) :: node 
    Type(PEnode_t),  Target,  Intent(INOUT) :: newnode 

    newnode%next_p      => node 
    newnode%prev_p      => node%prev_p
    node%prev_p%next_p  => newnode
    node%prev_p         => newnode 
   
    list%count = list%count+1

  End Subroutine insertNodeBefore

  Subroutine insertAfter(list, node, PE)
  !! Inserts a new PE  after a given node location in list 

    Class(PElist_t), Target,  Intent(INOUT) :: list
    Type(PEnode_t),  Pointer, Intent(INOUT) :: node 
    Class(entity_t),          Intent(IN)    :: PE 

    Call list%insertBefore(node%next_p, PE)
  
  End Subroutine insertAfter

  Subroutine insertNodeAfter(list, node, newnode)
  !! Inserts a complete node after a given node location in list 

    Class(PElist_t), Target, Intent(INOUT) :: list
    Type(PEnode_t),  Pointer,Intent(INOUT) :: node 
    Type(PEnode_t),  Target, Intent(INOUT) :: newnode

    Call list%insertNodeBefore(node%next_p, newNode)
  
  End Subroutine insertNodeAfter

  Subroutine append(list, PE)
  !! Appends a new PE value at the end of the list

    Class(PElist_t), Target, Intent(INOUT) :: list 
    Class(entity_t),         Intent(IN)    :: PE 

    Call list%insertBefore(list%head, PE) 

  End Subroutine append

  Subroutine appendNode(list, newnode)
  !! Appends a new node at the end of the list

    Class(PElist_t), Target, Intent(INOUT) :: list 
    Type(PEnode_t),  Target, Intent(INOUT) :: newnode 

    Call list%insertNodeBefore(list%head, newnode) 

  End Subroutine appendNode

  Subroutine prepend(list, PE)
  !! Appends a new PE value at the start of the list

    Class(PElist_t), Target, Intent(INOUT) :: list 
    Class(entity_t),         Intent(IN)    :: PE 

    Type(PEnode_t), Pointer :: current

    current=>list%first()
    Call list%insertBefore(current, PE)
           
  End Subroutine prepend

  Subroutine prependNode(list, newnode)
  !! Appends a new node at the start of the list

    Class(PElist_t), Target, Intent(INOUT) :: list 
    Type(PEnode_t),  Target, Intent(INOUT) :: newnode 

    Type(PEnode_t), Pointer :: current

    current=>list%first()
    Call list%insertNodeBefore(current, newnode)
           
  End Subroutine prependNode

  Subroutine removeNode(list, node, keep)
  !! Removes a given node from the list

    Class(PElist_t),           Intent(INOUT) :: list
    Type(PEnode_t),  Pointer,  Intent(INOUT) :: node
    Logical,         Optional, Intent(IN)    :: keep

    Logical :: keepnode

    keepnode = .FALSE.
    If (PRESENT(keep)) keepnode=keep

    If (ASSOCIATED(node, list%head)) Then
      Print *," WARNING - Tried to remove sentinel node" 
      RETURN
    EndIf
    node%next_p%prev_p => node%prev_p
    node%prev_p%next_P => node%next_p
    If (ASSOCIATED(node, list%head)) Then
      list%head => list%head%next_p
    EndIf

    If (.NOT. keepnode) Then
      If(ALLOCATED(node%PE)) DEALLOCATE(node%PE)
      If (ASSOCIATED(node)) DEALLOCATE(node)
    EndIf

    list%count = list%count - 1
    If (list%count < 0) list%count = 0

  End Subroutine removeNode

  Subroutine removeLast(list, keep)
  !! Removes the last node from the list

    Class(PElist_t),           Intent(INOUT) :: list
    Logical,         Optional, Intent(IN)    :: keep

    Type(PEnode_t), Pointer :: current

    current=>list%last()
    If (PRESENT(keep)) Then
      Call list%removeNode(current, keep)
    Else
      Call list%removeNode(current)
    EndIf

  End Subroutine removeLast

  Subroutine removeFirst(list, keep)
  !! Removes the first node from the list

    Class(PElist_t),           Intent(INOUT) :: list
    Logical,         Optional, Intent(IN)    :: keep

    Type(PEnode_t), Pointer :: current

    current => list%first()
    If (PRESENT(keep)) Then
      Call list%removeNode(current,keep)
    Else
      Call list%removeNode(current)
    EndIf

  End Subroutine removeFirst

  Function listSize(list) Result(size)
  !! Returns the current number of nodes in the list

    Class(PElist_t), Intent(IN) :: list
    Integer                     :: size 

    size = list%count
    If (size < 0) size = 0
 
  End Function listSize 

  Subroutine deallocPElistType(list)
  !! Completely deallocates an allocatable PE list case
  !! Dont use for non-allocatable lists

    Type(PElist_t), Allocatable, Target, Intent(INOUT) :: list


    Call list%delete()
    If (ALLOCATED(list)) DEALLOCATE(list)

  End Subroutine deallocPElistType

  Subroutine deletePElist(list)

    Type(PElist_t), Intent(INOUT) :: list
    Call list%delete()
 
  End Subroutine deletePElist 

  Subroutine clear(list, keep)
 !! Remove nodes from last to first. Keep data but destroy links if
 !! keep is present and .TRUE.

    Class(PElist_t),           Intent(INOUT) :: list
    Logical,         Optional, Intent(IN)    :: keep

    Type(PEnode_t), Pointer                :: current


     Do
       If (list%isEmpty()) EXIT
       current=>list%last()
       If (PRESENT(keep)) Then 
         Call list%removeNode(current,keep)
       Else
         Call list%removeNode(current)
       EndIf
     EndDo  
 
  End Subroutine clear

  Subroutine delete(list)
  !! Deletes the entire list contents
    
    Class(PElist_t), Intent(INOUT) :: list

    Call list%clear()
    Call delete_sentinel(list)

  End Subroutine delete 

  Subroutine delete_sentinel(list)
  !! Deletes the sentinel or head node

    Class(PElist_t), Intent(INOUT) :: list

     If (ALLOCATED(list%head%PE)) DEALLOCATE(list%head%PE)
     If (ASSOCIATED(list%head))   DEALLOCATE(list%head)

  End Subroutine delete_sentinel

  Function sentinel(list)
  !! Returns pointer to sentinel or head node

    Class(PElist_t),        Intent(INOUT) :: list
    Type(PEnode_t), Pointer               :: sentinel

    sentinel => list%head

  End Function sentinel

  Function isEmpty(list)
  !! Returns a LOGICAL .TRUE. if list is empty (list size = 0)
    Class(PElist_t), Intent(IN) :: list
    Logical                     :: isEmpty

    isEmpty = .FALSE.
    isEmpty = (list%size() == 0)

  End Function isEmpty

  Subroutine printPElist(list, ounit)
  !! Prints contents of each list node to stdout or a user supplied output unit

    Class(PElist_t), Target,   Intent(IN) :: list
    Integer,         Optional, Intent(IN) :: ounit

    Type(PEnode_t), Pointer :: node

    Integer :: i, nl

    Integer :: ou

    ou = stdout
    If (PRESENT(ounit)) ou = ounit

    If (ou == stdout) Then
      Write(stdout,*) ''
      Write(stdout,*) '(" *****  Printing Entity List To stdout  *****")'
      Write(stdout,*) ''
    End If

    node=>list%first()
    nl = list%size()
    Do i=1,nl
      Call node%PE%outputEntity(ou)
      node => node%next_p
    EndDo

  End Subroutine printPElist

  Subroutine moveToNode(list, node, num, back)
  !! Move through the list a specified number of nodes from the node arguments 
  !! current position when num is present. Otherwise move to the next node in
  !! the list. The head node is skipped if its the next node. The optional  
  !! LOGICAL back argument controls if movement is forward or backwards
  !! (BACK=.TRUE.) moves moves backwards from the specified node 

    Class(PElist_t),           Intent(INOUT) :: list
    Type(PEnode_t),  Pointer,  Intent(INOUT) :: node
    Integer,         Optional, Intent(IN)    :: num
    Logical,         Optional, Intent(IN)    :: back

    Logical :: moveback
    Integer :: nmoves, i

    moveback = .FALSE.
    If (PRESENT(back)) moveback = back

    nmoves = 1

    If (PRESENT(num)) nmoves=num
    nmoves = MAX(nmoves, 0)
   
    If (.NOT. moveback) Then

      If (nmoves == 1) Then
        node => node%next_p
        If (ASSOCIATED(node, list%head)) node=>node%next_p
      Else
        Do i=1,nmoves
          node=>node%next_p
          If (ASSOCIATED(node, list%head)) node=>node%next_p
        EndDo

      EndIf
 
    Else

      If (nmoves == 1) Then
        node => node%prev_p
        If (ASSOCIATED(node, list%head)) node=>node%prev_p
      Else
        Do i=1,nmoves
          node=>node%prev_p
          If (ASSOCIATED(node, list%head)) node=>node%prev_p
        EndDo
      EndIf

    EndIf

  End Subroutine moveToNode

  Subroutine copyTo(list, newlist)
  !! Copy list to newlist
    
    Class(PElist_t), Intent(IN)    :: list
    Type(PElist_t),  Intent(INOUT) :: newlist 

    Integer                  :: i, l1len
    Type(PEnode_t),  Pointer :: node
    
    l1len = list%count

    node=>list%first()
    If (.NOT. ASSOCIATED(newlist%head)) Then
      Call newlist%init() 
    EndIf
    Do i=1, l1len
      Call newlist%append(node%PE)
      node=>node%next()
    EndDo
    If (ASSOCIATED(node)) NULLIFY(node)

  End Subroutine copyTo

  Subroutine copyFrom(list, oldlist)
  !! Copy oldlist to list
  
    Class(PElist_t), Intent(INOUT) :: list
    Class(PElist_t), Intent(IN)    :: oldlist

    Call oldlist%copyTo(list)

  End Subroutine copyFrom 

  Subroutine assignPElists(list2, list1)
  !! Overloaded assignment operator to list1 to list2 

    Type(PElist_t), Intent(INOUT) :: list2
    Type(PElist_t), Intent(IN)    :: list1

     Call list1%copyTo(list2)
 
  End Subroutine assignPElists

  Subroutine mergeLists(list, list2, newlist)
  !! Merge two lists by either appending list2 to list or creating a new list

    Class(PElist_t),           Intent(INOUT) :: list
    !! this list
    Type(PElist_t),            Intent(IN)    :: list2
    !! A second list - appended to list if newlist is not present
    Type(PElist_t),  Optional, Intent(INOUT) :: newlist
    !! Optional new list to hold merged list
 
    Integer                 :: l1len, l2len, i
    Type(PEnode_t), Pointer :: node

    l1len = list%size()
    l2len = list2%count

    If (PRESENT(newlist)) Then
 
      If (.NOT.ASSOCIATED(newlist%head)) Then
        Call newlist%init()
      EndIf
      node=>list%first()
      Do i=1,l1len
        Call newlist%append(node%PE)
         node=>node%next()
      EndDo
      node => list2%first()
      Do i=1,l2len
        Call newlist%append(node%PE)
         node=>node%next()
      EndDo

    Else
      If (.NOT. ASSOCIATED(list%head)) Then
        Call list%init()
      EndIf
      node => list2%first()
      Do i=1,l2len
        Call list%append(node%PE)
         node=>node%next()
      EndDo
   
    EndIf

    If (ASSOCIATED(node)) NULLIFY(node)

  End Subroutine mergeLists

End Module PElist
