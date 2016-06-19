+++
title = "Rails tip: Dealing with out of sync migrations"
slug = "rails-tip-dealing-with-out-of-sync-migrations"
date = "2008-05-07T10:09:00+00:00"
draft = false

+++

Sometimes, for one embarrassing reason or another (usually involving chaotic branch merges...) a database migration can get leapfrogged. When this happens, it's tempting to renumber the leapfrogged migration, but that breaks any servers where the migration *didn't* get renumbered. Here's how I dealt with it recently:

<code>

    class MaybeOldMigration < ActiveRecord::Migration
      def self.up
        unless old_migration_applied?
          OldMigration.up
        end
      end

      def old_migration_applied?
        # Checks that the schema looks as it should
        # if the old migration got applied
      end
    end

</code>
Yeah, it's a hack, but it's a fairly robust hack.
