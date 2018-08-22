/* 
 *  Brian @ TMI.
 *  Written Feb 4 1992
 */

int use_remove(object player, object equip) {
  mixed *total_locations;
  string *potential_locations, *old_locations;
  object weap, arm;
  int wpriority, apriority, oweap, oarm;
  int check, i, j, k;
    total_locations = (mixed)equip->query_possibilities();
    wpriority = (int)equip->query_weapon_priority();
    apriority = (int)equip->query_armor_priority();
    for (i=0;i<sizeof(total_locations);i++) {
     if (equip->query_equipped()) potential_locations = (mixed)equip->
         query_where_equipped();
     else
      potential_locations = total_locations[i];
      for (j=0;j<sizeof(potential_locations);j++) {
       if (player->query_is_limb(potential_locations[j]) )
         {
        weap = (object)player->query_weapon(potential_locations[j]);
        arm = (object)player->query_armor(potential_locations[j]);
        if (weap) oweap = (int)weap->query_weapon_priority();
          else oweap = 0;
        if (arm) oarm = (int)arm->query_armor_priority();
          else oarm = 0;
        if ((weap == equip) || (arm == equip) || ((j > 0) && (check == 1)) ||
           (wpriority > oweap) || (apriority > oarm) )
         check = 1;
       else check = 0;
       }
      else check = 0;
      }
      if (check) {
        equip->set_where_equipped(potential_locations);
        for (j=0;j<sizeof(potential_locations);j++) {
          weap = (object)player->query_weapon(potential_locations[j]);
          arm = (object)player->query_armor(potential_locations[j]);
         if (weap) oweap = (int)weap->query_weapon_priority();
             else oweap = 0;
         if (arm) oarm = (int)arm->query_armor_priority();
             else oarm = 0;
         if (wpriority > oweap)
           {
         player->remove_weapon_from_limb(potential_locations[j]);
         player->equip_weapon_to_limb(equip,potential_locations[j]);
         if (weap) {
           old_locations = (mixed)weap->query_where_equipped();
           check = 0;
           for(k=0;k<sizeof(old_locations);k++) {
             if (
             ((object)player->query_weapon(old_locations[k]) == weap ||
               (object)player->query_armor(old_locations[k]) == weap)
                )
             check = 1;
             }
           if (!check) weap->set_not_equipped();
           }
           }
         if (apriority > oarm)
            /* as above  */
            {
         player->remove_armor_from_limb(potential_locations[j]);
         player->equip_armor_to_limb(equip, potential_locations[j]);
          if (arm) {
             old_locations = (mixed)weap->query_where_equipped();
             check = 0;
             for(k=0;k<sizeof(old_locations);k++) {
                if ( (object)player->query_weapon(old_locations[k]) == weap ||
                     (object)player->query_armor(old_locations[k]) == weap )
                 check = 1;
             }
             if (!check) arm->set_not_equipped();
          }
          }
          }
        return 1;
      }
     }
}

